module Main(main) where

import Data.Functor((<&>))
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Parser(parse)
import Language.Dynasty.Validate(validate)
import Language.Dynasty.Simplify(simplify)
import Language.Dynasty.Codegen(compile)
import Opts(Opts(..), getOpts)
import System.Process (system)
import System.Exit (exitFailure, exitWith)
import Control.Monad (when)

preludeEnv :: Set Text
preludeEnv =
  S.fromList
  [ "+", "-", "*", "/", "%"
  , "==", "<", "."
  , "!", "length", "show"
  , ">>=", "*>", "pure", "putStrLn", "print", "throw", "getArgs"
  , "fromCharCode", "toCharCode"
  , "getChar", "putChar" , "readFile"
  ]

pipeline :: Text -> FilePath -> Text -> Either Text Text
pipeline prelude path code =
  parse path code
  >>= \syn ->
    validate preludeEnv syn
    <&> simplify
    <&> compile prelude

writeJS :: Opts -> Text -> IO ()
writeJS Opts{..} code = do
  T.writeFile optsOutPath code
  when optsRun do
    system ("node --trace-uncaught " <> show optsOutPath <> foldMap ((" " <>) . show) optsArgs)
      >>= exitWith

run :: Opts -> IO ()
run opts@Opts{..} = do
  prelude <- T.readFile "vendored/prelude.js"
  code <- T.readFile optsPath

  case pipeline prelude optsPath code of
    Left err -> T.putStrLn err *> exitFailure
    Right js -> writeJS opts js

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
