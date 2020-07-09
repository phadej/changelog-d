-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative (optional, (<**>), (<|>))
import Data.Version        (showVersion)

import qualified Distribution.Parsec            as C
import qualified Distribution.Types.PackageName as C
import qualified Options.Applicative            as O

import ChangelogD (Opts (..), makeChangelog)

import Paths_changelog_d (version)

main :: IO ()
main = do
    opts <- O.execParser optsP'
    makeChangelog opts
  where
    optsP' = O.info (optsP <**> O.helper <**> versionP) $ mconcat
        [ O.fullDesc
        , O.progDesc "Concate changelog entries"
        , O.header "changelog-d - process changelog"
        ]

    versionP = O.infoOption (showVersion version)
        $ O.long "version" <> O.help "Show version"

-------------------------------------------------------------------------------
-- Options parser
-------------------------------------------------------------------------------

optsP :: O.Parser Opts
optsP = Opts
    <$> O.strArgument (O.metavar "<changelog.d>" <> O.value "changelog.d" <> O.showDefault <> O.help "Changelog directory")
    <*> shortP
    <*> optional packageNameP
    <*> optional prListP
  where
    shortP :: O.Parser Bool
    shortP =
        O.flag' True (O.long "short" <> O.help "Short output") <|>
        O.flag' False (O.long "long" <> O.help "Long output (with descriptions") <|>
        pure False

    packageNameP :: O.Parser C.PackageName
    packageNameP = O.option (O.eitherReader C.eitherParsec)
        (O.long "package" <> O.metavar "<pkgname>" <> O.help "Package filter for")

    prListP :: O.Parser FilePath
    prListP = O.strOption (O.long "prlog" <> O.metavar "<prlog>" <> O.help "Git log to check which PRs are not mentioned")
