module Main where

import Data.Function((&))

import Language.Dynasty.Frontend.Parser(parse)
import Language.Dynasty.Runtime.Eval(eval)
import Opts(Opts(..), getOpts)

getCode :: String -> IO String
getCode "-" = getContents
getCode path = readFile path

run :: Opts -> IO ()
run Opts{..} = do
  code <- getCode optsPath
  let useAST = if optsDumpAST then print else eval

  code
    & parse
    & either putStrLn useAST

main :: IO ()
main = getOpts >>= run
