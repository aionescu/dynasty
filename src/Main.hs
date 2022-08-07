module Main(main) where

import Data.Functor((<&>))
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.IO(BufferMode(NoBuffering), hSetBuffering, stdin, stdout)

import Language.Dynasty.Parser(parseModule)
import Language.Dynasty.Validate(validate)
import Language.Dynasty.Simplify(simplify)
import Language.Dynasty.Codegen(compile)
import Opts(Opts(..), getOpts)
import System.Exit(exitFailure)
import Control.Monad(join)
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), isExtensionOf)
import Data.Function ((&))
import Paths_dynasty (getDataDir)

getDyFiles :: FilePath -> IO [FilePath]
getDyFiles path =
  doesFileExist path >>= \case
    True -> pure [path | "dy" `isExtensionOf` path]
    _ -> (join <$>) . traverse (getDyFiles . (path </>)) =<< listDirectory path

readModule :: FilePath -> IO (FilePath, Text)
readModule path = (path,) <$> T.readFile path

run :: Opts -> IO ()
run Opts{..} = do
  coreDir <- (</> "core") <$> getDataDir
  runtime <- T.readFile $ coreDir </> "runtime.js"
  coreFiles <- getDyFiles coreDir
  userFiles <- getDyFiles optsPath
  modules <- traverse readModule $ coreFiles <> userFiles

  let
    outPath
      | null optsOutPath = optsPath </> "main.js"
      | otherwise = optsOutPath

  modules
    & traverse (uncurry parseModule)
    >>= validate
    <&> simplify
    <&> compile runtime
    & either
      (\e -> T.putStrLn e *> exitFailure)
      (T.writeFile outPath)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
