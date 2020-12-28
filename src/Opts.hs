module Opts(Opts(..), getOpts) where

import Options.Applicative

data Opts =
  Opts
  { optsDumpAST :: Bool
  , optsPath :: String
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> switch (long "dump-ast" <> help "Dump AST instead of running.")
  <*> strArgument (metavar "PATH" <> help "The source file to interpret.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Interprets Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser fullParser
