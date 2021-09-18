module Main where

import Data.Function((&))
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Frontend.Parser(parse)
import Language.Dynasty.Runtime.Eval(runTopLevel)
import Opts(Opts(..), getOpts)

getCode :: Text -> IO Text
getCode "-" = T.IO.getContents
getCode path = T.IO.readFile $ T.unpack path

run :: Opts -> IO ()
run Opts{..} = do
  code <- getCode optsPath
  let useAST = if optsDumpAST then print else runTopLevel optsArgs

  code
    & parse
    & either T.IO.putStrLn useAST

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
