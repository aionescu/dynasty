module Opts(Opts(..), getOpts) where

import Data.Text(Text)
import Options.Applicative

data Opts =
  Opts
  { optsDumpAST :: Bool
  , optsPath :: Text
  , optsArgs :: [Text]
  }

optsParser :: Parser Opts
optsParser =
  Opts
  <$> switch (long "dump-ast" <> help "Dump AST instead of running.")
  <*> strArgument (metavar "PATH" <> help "The source file to interpret.")
  <*> many (strArgument $ metavar "ARGS" <> help "Arguments to pass to the Dynasty program.")

fullParser :: ParserInfo Opts
fullParser =
  info
    (helper <*> optsParser)
    (fullDesc <> progDesc "Interprets Dynasty source files." <> header "Dynasty")

getOpts :: IO Opts
getOpts = execParser fullParser
