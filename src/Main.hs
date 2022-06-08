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
import System.FilePath ((</>), isExtensionOf, dropExtension, (<.>))
import Data.Function ((&))

isSingleFile :: FilePath -> IO Bool
isSingleFile f = ("dy" `isExtensionOf` f &&) <$> doesFileExist f

getDyFiles :: FilePath -> IO [FilePath]
getDyFiles path =
  doesFileExist path >>= \case
    True -> pure [path | "dy" `isExtensionOf` path]
    _ -> (join <$>) . traverse (getDyFiles . (path </>)) =<< listDirectory path

readModule :: FilePath -> IO (FilePath, Text)
readModule path = (path,) <$> T.readFile path

run :: Opts -> IO ()
run Opts{..} = do
  singleFile <- isSingleFile optsPath
  runtime <- T.readFile $ optsCoreDir </> "runtime.js"
  coreFiles <- getDyFiles optsCoreDir
  files <- getDyFiles optsPath
  modules <- traverse readModule $ coreFiles <> files

  let
    outPath =
      case (optsOutPath, singleFile) of
        ("", True) -> dropExtension optsPath <.> "js"
        ("", False) -> optsPath </> "main.js"
        _ -> optsOutPath

  modules
    & traverse (uncurry parseModule)
    >>= validate singleFile
    <&> simplify singleFile
    <&> compile runtime
    & either
      (\e -> T.putStrLn e *> exitFailure)
      (T.writeFile outPath)

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  getOpts >>= run
