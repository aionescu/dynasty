module Main(main) where

import Control.Monad(join)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Directory(listDirectory, doesDirectoryExist, doesFileExist)
import System.Exit(exitFailure)
import System.FilePath((</>), isExtensionOf)

import Language.Dynasty.Codegen(compile)
import Language.Dynasty.Lowering(lower)
import Language.Dynasty.NameResolution(resolveNames)
import Language.Dynasty.Opts(Opts(..), getOpts)
import Language.Dynasty.Parser(parse)
import Language.Dynasty.Utils(foldMapM, showT)
import Language.Dynasty.Validation(validate)
import Paths_dynasty(getDataDir)

showErr :: Text -> IO a
showErr e = T.putStrLn e *> exitFailure

getDyFiles :: FilePath -> IO [FilePath]
getDyFiles path =
  doesFileExist path >>= \case
    True -> pure [path | "dy" `isExtensionOf` path]
    _ -> listDirectory path >>= foldMapM (getDyFiles . (path </>))

parseDir :: Text -> FilePath -> IO [FilePath]
parseDir label path =
  doesDirectoryExist path >>= \case
    True -> getDyFiles path
    _ ->
      doesFileExist path >>= \case
        True -> showErr $ "Error: " <> label <> " must be a directory. " <> showT path <> " is a file."
        _ -> showErr $ "Error: " <> label <> " " <> showT path <> " does not exist."

run :: Opts -> IO ()
run opts = do
  coreDir <- (</> "core") <$> getDataDir
  runtime <- T.readFile $ coreDir </> "runtime.js"
  coreFiles <- parseDir "Core dir" coreDir
  projFiles <- parseDir "Project dir" opts.projPath

  modules <- traverse (join $ curry $ traverse T.readFile) $ coreFiles <> projFiles
  modules
    & parse
    >>= validate
    >>= resolveNames
    <&> lower
    <&> compile runtime
    & either showErr (T.writeFile opts.outPath)

main :: IO ()
main = getOpts >>= run
