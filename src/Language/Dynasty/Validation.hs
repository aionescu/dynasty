module Language.Dynasty.Validation(validate) where

import Control.Monad((>=>))
import Control.Monad.Except(throwError)
import Data.Containers.ListUtils(nubOrd)
import Data.Foldable(toList)
import Data.Functor((<&>))
import Data.Graph qualified as G
import Data.Maybe(fromJust)
import Data.Text(Text)
import Data.Text qualified as T

import Language.Dynasty.Syntax(Module(..), Program(..))
import Language.Dynasty.Utils(findDup, showT)

type Valid = Either Text

modulesUnique :: Program -> Valid Program
modulesUnique p@Program{..} =
  p <$ findDup (moduleName <$> programModules) ("Duplicate definition of module " <>)

findMainModule :: Program -> Valid Program
findMainModule p@Program{..} =
  case filter (elem "main" . fmap fst . moduleBindings) programModules of
    [] -> throwError "No main module found"
    [Module{..}] -> pure p{programMainModule = moduleName}
    ms -> throwError $ "Multiple suitable main modules found: " <> T.intercalate ", " (moduleName <$> ms)

topSortModules :: Program -> Valid Program
topSortModules p@Program{..} =
  unForest (G.scc g) <&> \ms -> p{programModules = ms, programReachable = reachable}
  where
    unForest [] = pure []
    unForest (G.Node v [] : ts) = (fst' (getNode v) :) <$> unForest ts
    unForest (t : _) = throwError $ "Import cycle: " <> showT (toList t)

    fst' (a, _, _) = a
    snd' (_, b, _) = b
    (g, getNode, getVertex) = G.graphFromEdges $ edge <$> programModules
    edge m@Module{..} = (m, moduleName, nubOrd moduleImports)
    reachable = snd' . getNode <$> G.reachable g (fromJust $ getVertex programMainModule)

validate :: Program -> Valid Program
validate = modulesUnique >=> findMainModule >=> topSortModules
