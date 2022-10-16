module Language.Dynasty.Validation(validate) where

import Control.Monad((>=>))
import Control.Monad.Except(throwError)
import Data.Containers.ListUtils(nubOrd)
import Data.Foldable(toList)
import Data.Functor((<&>))
import Data.Graph qualified as G
import Data.Maybe(fromJust)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T

import Language.Dynasty.Syntax(Module(..), Program(..))
import Language.Dynasty.Utils(findDup)

type Valid = Either Text

modulesUnique :: Program -> Valid Program
modulesUnique p =
  p <$ findDup ((.name) <$> p.modules) ("Duplicate definition of module " <>)

findMainModule :: Program -> Valid Program
findMainModule p =
  case filter (elem "main" . fmap fst . (.bindings)) p.modules of
    [] -> throwError "No main module found"
    [m] -> pure p{main = m.name}
    ms -> throwError $ "Multiple suitable main modules found: " <> T.intercalate ", " ((.name) <$> ms)

topSortModules :: Program -> Valid Program
topSortModules p =
  unForest (G.scc g) <&> \ms -> p{modules = ms}
  where
    unForest [] = pure []
    unForest (G.Node v [] : ts)
      | S.member name reachable = (m :) <$> unForest ts
      | otherwise = unForest ts
      where
        (m, name, _) = getNode v
    unForest (t : _) =
      throwError $ "Import cycle: " <> T.intercalate " -> " (getName . getNode <$> toList t)

    getName (_, n, _) = n
    (g, getNode, getVertex) = G.graphFromEdges $ edge <$> p.modules
    edge m = (m, m.name, nubOrd m.imports)
    reachable = S.fromList $ getName . getNode <$> G.reachable g (fromJust $ getVertex p.main)

validate :: Program -> Valid Program
validate = modulesUnique >=> findMainModule >=> topSortModules
