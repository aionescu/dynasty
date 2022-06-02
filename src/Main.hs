module Main where

import Data.Function((&))
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
preludeEnv = S.fromList ["+", ">>=", "*>", "pure", "print"]

getCode :: FilePath -> IO Text
getCode "-" = T.getContents
getCode path = T.readFile path

pipeline :: Text -> Text -> Either Text Text
pipeline prelude code =
  code
  & parse
  >>= \syn ->
    validate @(Either Text) preludeEnv syn
    <&> simplify
    <&> compile prelude

runOrWrite :: Opts -> Text -> IO ()
runOrWrite Opts{..} code = do
  T.writeFile optsOutPath code
  when optsRun do
    system ("node " <> show optsOutPath <> foldMap ((" " <>) . show) optsArgs)
      >>= exitWith

run :: Opts -> IO ()
run opts@Opts{..} = do
  code <- getCode optsPath
  prelude <- T.readFile "vendored/prelude.js"

  case pipeline prelude code of
    Left err -> T.putStrLn err *> exitFailure
    Right js -> runOrWrite opts js

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
