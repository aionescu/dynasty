module Main where

import Data.Function((&))
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Parser(parse)
import Language.Dynasty.Validate(validate)
import Language.Dynasty.Simplify(simplify)
import Opts(Opts(..), getOpts)
import Utils(showT)
import Data.Bifunctor (second)

prelude :: Set Text
prelude =
  S.fromList
  [ "prelude"
  , "+"
  , "-"
  , "*"
  , "/"
  , "%"
  , "^"
  , "<$>"
  , "pure"
  , "*>"
  , ">>="
  , "getLine"
  , "print"
  , "putStrLn"
  , "show"
  , "=="
  , "."
  , "&"
  , "trace"
  , "typeOf"
  , "<"
  , "readFile"
  , "getChar"
  , "putChar"
  , "getArgs"
  , "charToNum"
  , "numToChar"
  , ";"
  , "explode"
  , "implode"
  ]

getCode :: Text -> IO Text
getCode "-" = T.getContents
getCode path = T.readFile $ T.unpack path

pipeline :: Text -> Either Text Text
pipeline code =
  code
  & parse
  >>= \syn ->
    validate @(Either Text) prelude syn
    & second (\s -> showT s <> "\n\n" <> showT (simplify s))

run :: Opts -> IO ()
run Opts{..} = do
  code <- getCode optsPath

  pipeline code & either T.putStrLn T.putStrLn

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
