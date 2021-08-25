module Main where

import Data.Function((&))
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Frontend.Parser(parse)
import Language.Dynasty.Runtime.Prelude(setArgs)
import Language.Dynasty.Runtime.Eval(runTopLevel)
import Opts(Opts(..), getOpts)

getCode :: String -> IO String
getCode "-" = getContents
getCode path = readFile path

run :: Opts -> IO ()
run Opts{..} = do
  setArgs optsArgs

  code <- getCode optsPath
  let useAST = if optsDumpAST then print else runTopLevel

  code
    & parse
    & either putStrLn useAST

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
