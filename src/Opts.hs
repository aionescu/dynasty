module Opts(Opts(..), getOpts) where

import Options.Applicative
import System.Directory(getHomeDirectory)
import System.FilePath((</>))

data Opts =
  Opts
  { optsOutPath :: FilePath
  , optsCoreDir :: FilePath
  , optsPath :: FilePath
  }

optsParser :: FilePath -> Parser Opts
optsParser home =
  Opts
  <$> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value "" <> help "The name of the output file.")
  <*> strOption (long "core-dir" <> metavar "CORE_DIR" <> value (home </> ".dynasty/core") <> help "Specify an alternative directory for the Core modules.")
  <*> strArgument (metavar "PATH" <> value "." <> help "The directory to compile.")

fullParser :: FilePath -> ParserInfo Opts
fullParser home =
  info
    (helper <*> optsParser home)
    (fullDesc <> progDesc "Compiles Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser . fullParser =<< getHomeDirectory
