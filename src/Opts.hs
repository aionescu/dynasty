module Opts(Opts(..), getOpts) where

import Options.Applicative

data Opts =
  Opts
  { optsOutPath :: FilePath
  , optsPath :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value "" <> help "The name of the output file.")
  <*> strArgument (metavar "PATH" <> value "." <> help "The directory to compile.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Compiles Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser fullParser
