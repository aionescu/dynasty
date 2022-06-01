module Main where

import Data.Function((&))
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Frontend.Parser(parse)
import Language.Dynasty.Frontend.Validate(validate)
import Language.Dynasty.Runtime.Prelude(prelude)
import Opts(Opts(..), getOpts)

getCode :: Text -> IO Text
getCode "-" = T.IO.getContents
getCode path = T.IO.readFile $ T.unpack path

run :: Opts -> IO ()
run Opts{..} = do
  code <- getCode optsPath

  code
    & parse
    >>= validate (M.keysSet $ prelude optsArgs)
    & either T.IO.putStrLn pure

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
