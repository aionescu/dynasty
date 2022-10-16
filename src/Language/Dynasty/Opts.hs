{-# LANGUAGE StrictData #-}

module Language.Dynasty.Opts(Opts(..), getOpts) where

import Data.Maybe(fromMaybe)
import Options.Applicative
import System.FilePath((</>))

data Opts =
  Opts
  { outPath :: FilePath
  , projPath :: FilePath
  }

optsParser :: Parser Opts
optsParser =
  (\out proj -> Opts (fromMaybe (proj </> "main.js") out) proj)
  <$> optional (strOption $ short 'o' <> long "out-path" <> metavar "OUT_PATH" <> help "The name of the output file.")
  <*> strArgument (metavar "PATH" <> value "." <> help "The directory to compile.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Compiles Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser fullParser
