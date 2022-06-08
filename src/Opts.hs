module Opts(Opts(..), getOpts) where

import Options.Applicative

data Opts =
  Opts
  { optsOutPath :: FilePath
  , optsCoreDir :: FilePath
  , optsPath :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value "" <> help "The name of the output file.")
  <*> strOption (long "core-dir" <> metavar "CORE_DIR" <> value "core" <> help "Specify an alternative directory for the Core modules.")
  <*> strArgument (metavar "PATH" <> help "The directory to compile.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Interprets Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser fullParser
