module Main(main) where

import Control.Monad(join)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Maybe(fromMaybe)
import Data.Text(Text)
import Data.Text.IO qualified as T
import System.Directory(listDirectory, doesDirectoryExist, doesFileExist)
import System.Exit(exitFailure)
import System.FilePath((</>), isExtensionOf)

import Language.Dynasty.Parser(parseModule)
import Language.Dynasty.Validate(validate)
import Language.Dynasty.Simplify(simplify)
import Language.Dynasty.Codegen(compile)
import Opts(Opts(..), getOpts)
import Paths_dynasty(getDataDir)
import Utils(foldMapM, showT)

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
run Opts{..} = do
  coreDir <- (</> "core") <$> getDataDir
  runtime <- T.readFile $ coreDir </> "runtime.js"
  coreFiles <- parseDir "Core dir" coreDir
  userFiles <- parseDir "Project dir" optsPath
  modules <- traverse (join $ curry $ traverse T.readFile) $ coreFiles <> userFiles

  modules
    & traverse (uncurry parseModule)
    >>= validate
    <&> simplify
    <&> compile runtime
    & either showErr (T.writeFile $ fromMaybe (optsPath </> "main.js") optsOutPath)

main :: IO ()
main = getOpts >>= run
