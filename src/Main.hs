module Main(main) where

import Control.Monad(join)
import Data.Function((&))
import Data.Functor((<&>))
import Data.Maybe(fromMaybe)
import Data.Text.IO qualified as T
import System.Directory(listDirectory, doesFileExist)
import System.Exit(exitFailure)
import System.FilePath((</>), isExtensionOf)

import Language.Dynasty.Parser(parseModule)
import Language.Dynasty.Validate(validate)
import Language.Dynasty.Simplify(simplify)
import Language.Dynasty.Codegen(compile)
import Opts(Opts(..), getOpts)
import Paths_dynasty(getDataDir)
import Utils(foldMapM)

getDyFiles :: FilePath -> IO [FilePath]
getDyFiles path =
  doesFileExist path >>= \case
    True -> pure [path | "dy" `isExtensionOf` path]
    _ -> listDirectory path >>= foldMapM (getDyFiles . (path </>))

run :: Opts -> IO ()
run Opts{..} = do
  coreDir <- (</> "core") <$> getDataDir
  runtime <- T.readFile $ coreDir </> "runtime.js"
  coreFiles <- getDyFiles coreDir
  userFiles <- getDyFiles optsPath
  modules <- traverse (join $ curry $ traverse T.readFile) $ coreFiles <> userFiles

  modules
    & traverse (uncurry parseModule)
    >>= validate
    <&> simplify
    <&> compile runtime
    & either
      (\e -> T.putStrLn e *> exitFailure)
      (T.writeFile $ fromMaybe (optsPath </> "main.js") optsOutPath)

main :: IO ()
main = getOpts >>= run
