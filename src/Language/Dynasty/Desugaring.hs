module Language.Dynasty.Desugaring(desugar) where

import Control.Monad.Reader(Reader, ask, local, runReader)
import Data.Foldable(foldl', foldrM, toList)
import Data.Functor((<&>))
import Data.Graph qualified as G
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable(for)
import Data.Tuple(swap)

import Language.Dynasty.Core qualified as C
import Language.Dynasty.Syntax
import Language.Dynasty.Utils(imap, showT)

type VarGen = Reader Int

desugarPat :: C.Expr -> Pat -> ([(C.Expr, C.Check)], [(C.Expr, Id)])
desugarPat e = \case
  NumPat n -> ck e $ C.IsNum n
  StrPat s -> ck e $ C.IsStr s
  TupPat ps -> desugarPat e $ CtorPat "Tuple" ps
  ListPat ps -> desugarPat e $ unroll ps
  RecPat fs -> combine $ fs <&> \(f, p) ->
    ck e (C.HasField f) <#> let e' = C.Field e f in maybe (asg e' f) (desugarPat e') p
  VarPat v -> asg e v
  CtorPat c ps ->
    ck e (C.IsCtor c $ length ps)
    <#> combine (imap (\i -> desugarPat (C.Field e $ "$" <> showT i)) ps)
  Wildcard -> ([], [])
  As v p -> desugarPat e p <#> asg e v

  where
    unroll = foldr (\h t -> CtorPat "::" [h, t]) $ CtorPat "Nil" []

    ck e' c = ([(e', c)], [])
    asg e' v = ([], [(e', v)])

    combine = foldl' (<#>) ([], [])

    (c1, a1) <#> (c2, a2) = (c1 <> c2, a1 <> a2)
    infixr 6 <#>

desugarCase :: Id -> [(Pat, C.Expr)] -> C.Expr
desugarCase v bs = C.Case $ bs <&> \(p, e) ->
  desugarPat (C.Var $ Unqual v) p <&> \case
    [] -> e
    as -> C.Let [(False, swap <$> as)] e

desugarLam :: [Pat] -> Expr -> VarGen C.Expr
desugarLam [] e = desugarExpr e
desugarLam (p : ps) e =
  case p of
    Wildcard -> withFreshVar \v -> C.Lam v <$> desugarLam ps e
    VarPat v -> C.Lam v <$> desugarLam ps e
    _ -> withFreshVar \v -> C.Lam v <$> desugarExpr (Case (Var $ Unqual v) [(p, Lam ps e)])

freeVars :: C.Expr -> Set Id
freeVars C.NumLit{} = S.empty
freeVars C.StrLit{} = S.empty
freeVars (C.RecLit fs) = foldMap (freeVars . snd) fs
freeVars (C.CtorLit _ es) = foldMap freeVars es
freeVars (C.Var (Unqual v)) = S.singleton v
freeVars (C.Var Qual{}) = S.empty
freeVars (C.Field e _) = freeVars e
freeVars (C.Case bs) = foldMap (\(cs, e) -> foldMap (freeVars . fst) cs <> freeVars e) bs
freeVars (C.Lam v e) = freeVars e S.\\ S.singleton v
freeVars (C.App f a) = freeVars f <> freeVars a
freeVars (C.Let bs e) =
  (freeVars e <> foldMap (foldMap (freeVars . snd) . snd) bs)
  S.\\ foldMap (foldMap (S.singleton . fst) . snd) bs
freeVars (C.UnsafeJS _ vs _) = S.fromList vs

splitRecGroups :: [(Id, C.Expr)] -> C.BindingGroup
splitRecGroups bs = unForest $ G.scc g
  where
    vars = foldMap (S.singleton . fst) bs
    deps = bs <&> \(i, e) ->
      let free = freeVars e `S.intersection` vars
      in ((i `S.member` free, e), i, S.toList free)

    (g, getNode, _) = G.graphFromEdges deps

    unForest (G.Node (getNode -> ((False, e), i, _)) [] : ts) = nonRec [(i, e)] ts
    unForest (n : ts) =  (True, toList n <&> \(getNode -> ((_, e), i, _)) -> (i, e)) : unForest ts
    unForest [] = []

    nonRec acc (G.Node (getNode -> ((False, e), i, _)) [] : ts) = nonRec ((i, e) : acc) ts
    nonRec acc ts = (False, reverse acc) : unForest ts

desugarBindingGroup :: BindingGroup -> VarGen C.BindingGroup
desugarBindingGroup bs = splitRecGroups <$> traverse (traverse desugarExpr) bs

withFreshVar :: (Id -> VarGen a) -> VarGen a
withFreshVar f = do
  i <- ask
  local succ $ f $ "_" <> showT i

desugarExpr :: Expr -> VarGen C.Expr
desugarExpr (NumLit n) = pure $ C.NumLit n
desugarExpr (StrLit s) = pure $ C.StrLit s
desugarExpr (TupLit es) = C.CtorLit "Tuple" <$> traverse desugarExpr es
desugarExpr (ListLit es) =
  foldrM (\h t -> (\h' -> C.CtorLit "::" [h', t]) <$> desugarExpr h) (C.CtorLit "Nil" []) es
desugarExpr (RecLit es) = C.RecLit <$> for es \case
  (i, Nothing) -> pure (i, C.Var $ Unqual i)
  (i, Just e) -> (i,) <$> desugarExpr e
desugarExpr (Var v) = pure $ C.Var v
desugarExpr (RecField e f) = (`C.Field` f) <$> desugarExpr e
desugarExpr (CtorField e i) = (`C.Field` ("$" <> showT i)) <$> desugarExpr e
desugarExpr (Case e bs)
  | Var (Unqual v) <- e, T.head v == '_' = desugarCase v <$> traverse (traverse desugarExpr) bs
  | otherwise = withFreshVar \v ->
      (\e' -> C.Let [(False, [(v, e')])])
      <$> desugarExpr e
      <*> (desugarCase v <$> traverse (traverse desugarExpr) bs)
desugarExpr (Lam vs e) = desugarLam vs e
desugarExpr (LamCase bs) = withFreshVar \v -> C.Lam v <$> desugarExpr (Case (Var $ Unqual v) bs)
desugarExpr (CtorLit ctor es) = C.CtorLit ctor <$> traverse desugarExpr es
desugarExpr (App f a) = C.App <$> desugarExpr f <*> desugarExpr a
desugarExpr (Let [] e) = desugarExpr e
desugarExpr (Let bs e) = C.Let <$> desugarBindingGroup bs <*> desugarExpr e
desugarExpr (UnsafeJS whnf vs js) = pure $ C.UnsafeJS whnf vs js
desugarExpr (Do n stmts e) = desugarExpr $ foldr bind' e stmts
  where
    bind' (p, a) e' = (Var n `App` a) `App` Lam [fromMaybe Wildcard p] e'

desugarModule :: Module -> C.Module
desugarModule Module{..} =
  C.Module
  { moduleBindings = desugarBindingGroup moduleBindings `runReader` 0
  , moduleExports = fst <$> moduleBindings
  , ..
  }

desugar :: Program -> ([C.Module], Id)
desugar Program{..} =
  ( desugarModule <$> filter ((`S.member` reachable) . moduleName) programModules
  , programMainModule
  )
  where
    reachable = S.fromList programReachable
