{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wall #-}
-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
--
-- This is a demo application of how you can make Cabal-like
-- file formatter.
--
module ChangelogD (makeChangelog, Opts (..)) where

import Control.Exception       (Exception (..))
import Control.Monad           (when)
import Data.Char               (isSpace)
import Data.Foldable           (for_, traverse_)
import Data.Function           (on)
import Data.Functor.Identity   (Identity (..))
import Data.Generics.Lens.Lite (field)
import Data.List               (sort, sortBy)
import Data.Maybe              (isJust)
import Data.Proxy              (Proxy (..))
import Data.Set                (Set)
import Data.Traversable        (for)
import GHC.Generics            (Generic)
import System.Directory        (listDirectory)
import System.Exit             (exitFailure)
import System.FilePath         ((</>))
import System.IO               (hPutStrLn, stderr)

import qualified Cabal.Parse                     as Parse
import qualified Data.ByteString                 as BS
import qualified Data.Set                        as Set
import qualified Distribution.CabalSpecVersion   as C
import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.Newtype     as C
import qualified Distribution.FieldGrammar       as C
import qualified Distribution.Fields             as C
import qualified Distribution.Parsec             as C
import qualified Distribution.Parsec.Newtypes    as C
import qualified Distribution.Pretty             as C
import qualified Distribution.Simple.Utils       as C
import qualified Distribution.Types.PackageName  as C
import qualified Text.PrettyPrint                as PP


exitWithExc :: Exception e => e -> IO a
exitWithExc e = do
    hPutStrLn stderr $ displayException e
    exitFailure

makeChangelog :: Opts -> IO ()
makeChangelog Opts {..} = do
    cfg <- do
        let filename = optDirectory </> "config"
        contents <- BS.readFile filename
        either exitWithExc return $ Parse.parseWith parseConfig filename contents

    existingContents <- traverse BS.readFile optExisting
    dirContents <- filter (not . isTmpFile) <$> listDirectory optDirectory
    entries <- for (filter (/= "config") $ sort dirContents) $ \name -> do
        let fp = optDirectory </> name
        contents <- BS.readFile fp
        either exitWithExc return $ Parse.parseWith parseEntry fp contents

    if null entries
    then traverse_ BS.putStr existingContents
    else do
        let PerSignificance significant other = groupBySignificance entries

        if null significant
        then
            for_ entries $ \entry ->
                putStr $ formatEntry cfg entry
        else do
            putStrLn "### Significant changes\n"

            for_ (sortBy ((packagesCmp `on` entryPackages) <> (flip compare `on` hasDescription)) significant) $ \entry ->
                putStr $ formatEntry cfg entry

            putStrLn "### Other changes\n"

            for_ (sortBy (flip compare `on` hasDescription) other) $ \entry ->
                putStr $ formatEntry cfg entry

        traverse_ BS.putStr existingContents

isTmpFile :: FilePath -> Bool
isTmpFile ('.' : _) = True
isTmpFile _ = False

packagesCmp :: Set C.PackageName -> Set C.PackageName -> Ordering
packagesCmp xs ys = compare (length xs) (length ys) <> compare xs ys

-------------------------------------------------------------------------------
-- Formatting
-------------------------------------------------------------------------------

formatEntry :: Cfg -> Entry -> String
formatEntry cfg Entry {..} =
    indent $ header ++ "\n" ++ description
  where
    indent = unlines . indent' . lines
    indent' []     = []
    indent' (x:xs) = ("- " ++ x) : map ("  " ++) xs

    github = "https://github.com/" ++ cfgOrganization cfg ++ "/" ++ cfgRepository cfg

    header = unwords $
        [ pkgs ++ entrySynopsis
        ] ++
        [ "[#" ++ show n ++ "](" ++ github ++ "/issues/" ++ show n ++ ")"
        | IssueNumber n <- Set.toList entryIssues
        ] ++
        [ "[#" ++ show n ++ "](" ++ github ++ "/pull/" ++ show n ++ ")"
        | IssueNumber n <- Set.toList entryPrs
        ]

    pkgs = concatMap (\pn -> "*" ++ C.unPackageName pn ++ "* ") $ Set.toList entryPackages

    description = maybe "" (\d -> "\n" ++ trim d ++ "\n\n") entryDescription

trim :: String -> String
trim = tr . tr where tr = dropWhile isSpace . reverse

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
  deriving (Eq, Ord, Show)

instance C.Parsec IssueNumber where
    parsec = do
        _ <- P.char '#'
        IssueNumber <$> P.integral

instance C.Pretty IssueNumber where
    pretty (IssueNumber n) = PP.char '#' PP.<> PP.int n

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

data Cfg = Cfg
    { cfgOrganization :: String
    , cfgRepository   :: String
    }
  deriving (Show, Generic)

parseConfig :: [C.Field C.Position] -> C.ParseResult Cfg
parseConfig fields0 = do
    traverse_ parseSection $ concat sections
    C.parseFieldGrammar C.cabalSpecLatest fields cfgGrammar
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult ()
    parseSection (C.MkSection (C.Name pos name) _ _) =
        C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ C.fromUTF8BS name

cfgGrammar :: C.ParsecFieldGrammar Cfg Cfg
cfgGrammar = Cfg
    <$> C.uniqueFieldAla   "organization" C.Token (field @"cfgOrganization")
    <*> C.uniqueFieldAla   "repository"   C.Token (field @"cfgRepository")

-------------------------------------------------------------------------------
-- Entry
-------------------------------------------------------------------------------

data Entry = Entry
    { entrySynopsis     :: String
    , entryDescription  :: Maybe String
    , entryPackages     :: Set C.PackageName
    , entryPrs          :: Set IssueNumber
    , entryIssues       :: Set IssueNumber
    , entrySignificance :: Significance
    }
  deriving (Show, Generic)

hasDescription :: Entry -> Bool
hasDescription = isJust . entryDescription

parseEntry :: [C.Field C.Position] -> C.ParseResult Entry
parseEntry fields0 = do
    traverse_ parseSection $ concat sections
    e <- C.parseFieldGrammar C.cabalSpecLatest fields entryGrammar
    when (null $ entrySynopsis e) $
        C.parseFatalFailure C.zeroPos "Synopsis cannot be empty"
    return e
  where
    (fields, sections) = C.partitionFields fields0

    parseSection :: C.Section C.Position -> C.ParseResult ()
    parseSection (C.MkSection (C.Name pos name) _ _) =
        C.parseWarning pos C.PWTUnknownSection $ "Unknown section " ++ C.fromUTF8BS name

entryGrammar :: C.ParsecFieldGrammar Entry Entry
entryGrammar = Entry
    <$> C.freeTextFieldDef "synopsis"                            (field @"entrySynopsis")
    <*> C.freeTextField    "description"                         (field @"entryDescription")
    <*> C.monoidalFieldAla "packages"     (alaSet C.NoCommaFSep) (field @"entryPackages")
    <*> C.monoidalFieldAla "prs"          (alaSet C.NoCommaFSep) (field @"entryPrs")
    <*> C.monoidalFieldAla "issues"       (alaSet C.NoCommaFSep) (field @"entryIssues")
    <*> C.optionalFieldDef "significance"                        (field @"entrySignificance") Other

-------------------------------------------------------------------------------
-- AlaSet
-------------------------------------------------------------------------------

newtype AlaSet sep b a = AlaSet { _getAlaSet :: Set a }
  deriving anyclass (C.Newtype (Set a))

alaSet :: sep -> Set a -> AlaSet sep (Identity a) a
alaSet _ = AlaSet

-- | More general version of 'alaSet'.
_alaSet' :: sep -> (a -> b) -> Set a -> AlaSet sep b a
_alaSet' _ _ = AlaSet

instance (C.Newtype a b, Ord a, C.Sep sep, C.Parsec b) => C.Parsec (AlaSet sep b a) where
    parsec   = C.pack . Set.fromList . map (C.unpack :: b -> a) <$> C.parseSep (hack (Proxy :: Proxy sep)) C.parsec

instance (C.Newtype a b, C.Sep sep, C.Pretty b) => C.Pretty (AlaSet sep b a) where
    pretty = C.prettySep (hack (Proxy :: Proxy sep)) . map (C.pretty . (C.pack :: a -> b)) . Set.toList . C.unpack

-- Someone (= me) forgot to export Distribution.Parsec.Newtypes.P
hack :: Proxy a -> proxy a
hack _ = undefined
