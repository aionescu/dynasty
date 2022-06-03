module Opts(Opts(..), getOpts) where

import Data.Text(Text)
import Options.Applicative
import System.FilePath (dropExtension)

data Opts =
  Opts
  { optsRun :: Bool
  , optsOutPath :: FilePath
  , optsPath :: FilePath
  , optsArgs :: [Text]
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> switch (long "run" <> help "Immediately run the resulting JS file.")
  <*> strOption (short 'o' <> long "out-path" <> metavar "OUT_PATH" <> value "" <> help "The name of the output file.")
  <*> strArgument (metavar "PATH" <> help "The source file to interpret.")
  <*> many (strArgument $ metavar "ARGS" <> help "Arguments to pass to the Dynasty program.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Interprets Dynasty source files." <> header "Dynasty")

setOutPath :: Opts -> Opts
setOutPath opts =
  case optsOutPath opts of
    "" -> opts { optsOutPath = dropExtension (optsPath opts) <> ".js" }
    _ -> opts

getOpts :: IO Opts
getOpts = setOutPath <$> execParser fullParser
