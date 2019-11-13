{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall #-}
-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
--
-- This is a demo application of how you can make Cabal-like
-- file formatter.
--
module ChangelogD (makeChangelog, Opts (..)) where

import Control.Exception (Exception (..))
import Data.Char         (isSpace)
import Data.Foldable     (for_, traverse_)
import Data.Function     (on)
import Data.List         (nub, sort, sortBy)
import Data.Traversable  (for)
import GHC.Generics      (Generic)
import System.Directory  (listDirectory)
import System.Exit       (exitFailure)
import System.FilePath   ((</>))
import System.IO         (hPutStrLn, stderr)

import qualified Data.ByteString                 as BS
import qualified Data.Set                        as Set
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Simple.Utils       as C
import qualified Distribution.Types.PackageName  as C
import qualified Text.PrettyPrint                as PP

import Data.Generics.Labels ()

import Cabal.Parse

exitWithExc :: Exception e => e -> IO a
exitWithExc e = do
    hPutStrLn stderr $ displayException e
    exitFailure

makeChangelog :: Opts -> IO ()
makeChangelog Opts {..} = do
    existingContents <- traverse BS.readFile optExisting
    dirContents <- filter (not . isTmpFile) <$> listDirectory optDirectory
    entries <- for (sort dirContents) $ \name -> do
        let fp = optDirectory </> name
        contents <- BS.readFile fp
        either exitWithExc return $ parseWith parseEntry fp contents

    if null entries
    then traverse_ BS.putStr existingContents
    else do
        let PerSignificance significant other = groupBySignificance entries

        if null significant
        then
            for_ entries $ \entry ->
                putStr $ formatEntry entry
        else do
            putStrLn "### Significant changes\n"

            for_ (sortBy (packagesCmp `on` entryPackages) significant) $ \entry ->
                putStr $ formatEntry entry

            putStrLn "### Other changes\n"

            for_ other $ \entry ->
                putStr $ formatEntry entry

        traverse_ BS.putStr existingContents

isTmpFile :: FilePath -> Bool
isTmpFile ('.' : _) = True
isTmpFile _ = False

packagesCmp :: [C.PackageName] -> [C.PackageName] -> Ordering
packagesCmp xs ys =
    compare (length xs') (length ys') <> compare xs' ys'
  where
    xs' = Set.fromList xs
    ys' = Set.fromList ys
-- packagesCmp ::

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

formatEntry :: Entry -> String
formatEntry Entry {..} =
    indent $ header ++ "\n" ++ description ++ "\n"
  where
    indent = unlines . indent' . lines
    indent' []     = []
    indent' (x:xs) = ("- " ++ x) : map ("  " ++) xs

    header = unwords $
        [ pkgs ++ entrySynopsis
        ] ++
        [ "[#" ++ show n ++ "](https://github.com/phadej/changelog-d/issues/" ++ show n ++ ")"
        | IssueNumber n <- entryIssues
        ] ++
        [ "[#" ++ show n ++ "](https://github.com/phadej/changelog-d/pull/" ++ show n ++ ")"
        | IssueNumber n <- entryPrs
        ]

    pkgs = concatMap (\pn -> "*" ++ C.unPackageName pn ++ "* ") $ nub $ sort entryPackages

    description = maybe "" (\d -> "\n" ++ trim d ++ "\n") entryDescription

    trim = dropWhile isSpace . reverse .  dropWhile isSpace . reverse

-------------------------------------------------------------------------------
-- Options
-------------------------------------------------------------------------------

data Opts = Opts
    { optExisting  :: Maybe FilePath
    , optDirectory :: FilePath
    }
  deriving (Show)

-------------------------------------------------------------------------------
-- Auxiliary types
-------------------------------------------------------------------------------

data Significance = Significant | Other
  deriving (Eq, Show)

data PerSignificance a = PerSignificance a a

groupBySignificance :: [Entry] -> PerSignificance [Entry]
groupBySignificance xs = PerSignificance
    (filter ((== Significant) . entrySignificance) xs)
    (filter ((== Other) . entrySignificance) xs)

instance C.Parsec Significance where
    parsec = do
        token <- C.parsecToken
        case token of
            "significant" -> return Significant
            "other"       -> return Other
            _             -> fail $ "Unknown significance: " ++ token

instance C.Pretty Significance where
    pretty Significant = PP.text "significant"
    pretty Other       = PP.text "other"

newtype IssueNumber = IssueNumber Int
  deriving (Eq, Show)

instance C.Parsec IssueNumber where
    parsec = do
        _ <- P.char '#'
        IssueNumber <$> P.integral

instance C.Pretty IssueNumber where
    pretty (IssueNumber n) = PP.char '#' PP.<> PP.int n

-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------

data Entry = Entry
    { entrySynopsis     :: String
    , entryDescription  :: Maybe String
    , entryPackages     :: [C.PackageName]
    , entryPrs          :: [IssueNumber]
    , entryIssues       :: [IssueNumber]
    , entrySignificance :: Significance
    }
  deriving (Show, Generic)

parseEntry :: [C.Field C.Position] -> C.ParseResult Entry
parseEntry fields0 = do
    traverse_ parseSection $ concat sections
    C.parseFieldGrammar C.cabalSpecLatest fields entryGrammar
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult ()
    parseSection (C.MkSection (C.Name pos name) _ _) =
        C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ C.fromUTF8BS name

entryGrammar :: C.ParsecFieldGrammar Entry Entry
entryGrammar = Entry
    <$> C.freeTextFieldDef "synopsis"                               #entrySynopsis
    <*> C.freeTextField    "description"                            #entryDescription
    <*> C.monoidalFieldAla "packages"     (C.alaList C.NoCommaFSep) #entryPackages
    <*> C.monoidalFieldAla "prs"          (C.alaList C.NoCommaFSep) #entryPrs
    <*> C.monoidalFieldAla "issues"       (C.alaList C.NoCommaFSep) #entryIssues
    <*> C.optionalFieldDef "significance"                           #entrySignificance Other
