module Language.Dynasty.Desugaring(desugar) where

import Control.Monad.Reader(Reader, ask, local, runReader)
import Data.Foldable(foldl', foldrM, toList)
import Data.Functor((<&>))
import Data.Graph qualified as G
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import Data.Set qualified as S'
import Data.Text qualified as T
import Data.Traversable(for)
import Data.Tuple(swap)

import Language.Dynasty.Core
import Language.Dynasty.Syntax(Id, Name(..))
import Language.Dynasty.Syntax qualified as S
import Language.Dynasty.Utils(imap, showT)

type VarGen = Reader Int

desugarPat :: Expr -> S.Pat -> ([(Expr, Check)], [(Expr, Id)])
desugarPat e = \case
  S.NumPat n -> ck e $ IsNum n
  S.StrPat s -> ck e $ IsStr s
  S.TupPat ps -> desugarPat e $ S.CtorPat "Tuple" ps
  S.ListPat ps -> desugarPat e $ unroll ps
  S.RecPat fs -> combine $ fs <&> \(f, p) ->
    ck e (HasField f) <#> let e' = Field e f in maybe (asg e' f) (desugarPat e') p
  S.VarPat v -> asg e v
  S.CtorPat c ps ->
    ck e (IsCtor c $ length ps)
    <#> combine (imap (\i -> desugarPat (Field e $ "$" <> showT i)) ps)
  S.Wildcard -> ([], [])
  S.As v p -> desugarPat e p <#> asg e v

  where
    unroll = foldr (\h t -> S.CtorPat "::" [h, t]) $ S.CtorPat "Nil" []

    ck e' c = ([(e', c)], [])
    asg e' v = ([], [(e', v)])

    combine = foldl' (<#>) ([], [])

    (c1, a1) <#> (c2, a2) = (c1 <> c2, a1 <> a2)
    infixr 6 <#>

desugarCase :: Id -> [(S.Pat, Expr)] -> Expr
desugarCase v bs = Case $ bs <&> \(p, e) ->
  let (cs, as) = desugarPat (Var $ Unqual v) p
  in (cs, Let [(False, swap <$> as)] e)

desugarLam :: [S.Pat] -> S.Expr -> VarGen Expr
desugarLam [] e = desugarExpr e
desugarLam (p : ps) e =
  case p of
    S.Wildcard -> withFreshVar \v -> Lam v <$> desugarLam ps e
    S.VarPat v -> Lam v <$> desugarLam ps e
    _ -> withFreshVar \v -> Lam v <$> desugarExpr (S.Case (S.Var $ S.Unqual v) [(p, S.Lam ps e)])

freeVars :: Expr -> Set Id
freeVars NumLit{} = S'.empty
freeVars StrLit{} = S'.empty
freeVars (RecLit fs) = foldMap (freeVars . snd) fs
freeVars (CtorLit _ es) = foldMap freeVars es
freeVars (Var (Unqual v)) = S'.singleton v
freeVars (Var Qual{}) = S'.empty
freeVars (Field e _) = freeVars e
freeVars (Case bs) = foldMap (\(cs, e) -> foldMap (freeVars . fst) cs <> freeVars e) bs
freeVars (Lam v e) = freeVars e S'.\\ S'.singleton v
freeVars (App f a) = freeVars f <> freeVars a
freeVars (Let bs e) =
  (freeVars e <> foldMap (foldMap (freeVars . snd) . snd) bs)
  S'.\\ foldMap (foldMap (S'.singleton . fst) . snd) bs
freeVars (UnsafeJS _ vs _) = S'.fromList vs

splitRecGroups :: [(Id, Expr)] -> BindingGroup
splitRecGroups bs = unTree <$> G.scc g
  where
    vars = foldMap (S'.singleton . fst) bs
    deps = bs <&> \(i, e) ->
      let free = freeVars e `S'.intersection` vars
      in ((i `S'.member` free, e), i, S'.toList free)

    (g, getNode, _) = G.graphFromEdges deps

    unTree (G.Node v []) = (r, [(i, e)])
      where
        ((r, e), i, _) = getNode v
    unTree n = (True, toList n <&> \(getNode -> ((_, e), i, _)) -> (i, e))

desugarBindingGroup :: S.BindingGroup -> VarGen BindingGroup
desugarBindingGroup bs = splitRecGroups <$> traverse (traverse desugarExpr) bs

withFreshVar :: (Id -> VarGen a) -> VarGen a
withFreshVar f = do
  i <- ask
  local succ $ f $ "_" <> showT i

desugarExpr :: S.Expr -> VarGen Expr
desugarExpr (S.NumLit n) = pure $ NumLit n
desugarExpr (S.StrLit s) = pure $ StrLit s
desugarExpr (S.TupLit es) = CtorLit "Tuple" <$> traverse desugarExpr es
desugarExpr (S.ListLit es) =
  foldrM (\h t -> (\h' -> CtorLit "::" [h', t]) <$> desugarExpr h) (CtorLit "Nil" []) es
desugarExpr (S.RecLit es) = RecLit <$> for es \case
  (i, Nothing) -> pure (i, Var $ Unqual i)
  (i, Just e) -> (i,) <$> desugarExpr e
desugarExpr (S.Var v) = pure $ Var v
desugarExpr (S.RecField e f) = (`Field` f) <$> desugarExpr e
desugarExpr (S.CtorField e i) = (`Field` ("$" <> showT i)) <$> desugarExpr e
desugarExpr (S.Case e bs)
  | S.Var (S.Unqual v) <- e, T.head v == '_' = desugarCase v <$> traverse (traverse desugarExpr) bs
  | otherwise = withFreshVar \v ->
      (\e' -> Let [(False, [(v, e')])])
      <$> desugarExpr e
      <*> (desugarCase v <$> traverse (traverse desugarExpr) bs)
desugarExpr (S.Lam vs e) = desugarLam vs e
desugarExpr (S.LamCase bs) = withFreshVar \v -> Lam v <$> desugarExpr (S.Case (S.Var $ S.Unqual v) bs)
desugarExpr (S.CtorLit ctor es) = CtorLit ctor <$> traverse desugarExpr es
desugarExpr (S.App f a) = App <$> desugarExpr f <*> desugarExpr a
desugarExpr (S.Let [] e) = desugarExpr e
desugarExpr (S.Let bs e) = Let <$> desugarBindingGroup bs <*> desugarExpr e
desugarExpr (S.UnsafeJS whnf vs js) = pure $ UnsafeJS whnf vs js
desugarExpr (S.Do n stmts e) = desugarExpr $ foldr bind' e stmts
  where
    bind' (p, a) e' = (S.Var n `S.App` a) `S.App` S.Lam [fromMaybe S.Wildcard p] e'

desugarModule :: S.Module -> Module
desugarModule S.Module{..} =
  Module
  { moduleBindings = desugarBindingGroup moduleBindings `runReader` 0
  , moduleExports = fst <$> moduleBindings
  , ..
  }

desugar :: S.Program -> ([Module], Id)
desugar S.Program{..} =
  ( desugarModule <$> filter ((`S'.member` reachable) . S.moduleName) programModules
  , programMainModule
  )
  where
    reachable = S'.fromList programReachable
