-- |
-- License: GPL-3.0-or-later
-- Copyright: Oleg Grenrus
module Main (main) where

import Control.Applicative ((<**>), optional)
import Data.Version        (showVersion)

import qualified Data.ByteString     as BS
import qualified Options.Applicative as O

import ChangelogD (makeChangelog, Opts (..))

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
    <$> optional (O.strOption (O.short 'o' <> O.long "old" <> O.metavar "Changelog" <> O.help "Existing changelog to concatenate"))
    <*> O.strArgument (O.metavar "<changelog.d>" <> O.value "changelog.d" <> O.showDefault <> O.help "Changelog directory")
