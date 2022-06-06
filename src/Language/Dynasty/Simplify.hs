module Language.Dynasty.Simplify(simplify) where

import Control.Monad.Reader(MonadReader, ask, local, runReader)
import Data.Array qualified as A
import Data.Bifunctor(second)
import Data.Foldable(foldl', foldrM)
import Data.Graph(Tree(..))
import Data.Graph qualified as G
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S'
import Data.Text qualified as T
import Data.Traversable(for)
import Data.Tuple(swap)
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident)
import Language.Dynasty.Syntax qualified as S
import Language.Dynasty.Core
import Utils(showT)
import Data.Functor ((<&>))

-- This module converts the `Syntax` AST into `Core`.
-- The main differences between Syntax and Core are:
-- * Core differentiates between Let and LetRec.
-- * The are no patterns in Core. Instead, `Case` stores a `Dig` for each branch, which is a
--   deconstruction of a pattern into its "checks" and "assignments".
-- * Lambdas have exactly one identifier argument in Core. Lambdas with pattern arguments and
--   LambdaCase are simplified into a series of Lambdas and Cases.

simplifyPat :: Expr -> S.Pat -> (Vector (Expr, Check), Vector (Expr, Ident))
simplifyPat e = \case
  S.NumLit n -> ck e $ IsNum n
  S.StrLit s -> ck e $ IsStr s
  S.Tuple ps -> simplifyPat e $ S.App (S.Ctor "Tuple") ps
  S.List ps -> simplifyPat e $ unroll ps
  S.Record fs -> combine $ fs <&> \(f, p) ->
    ck e (HasField f) <#> let e' = Field e f in maybe (asg e' f) (simplifyPat e') p
  S.Var v -> asg e v
  S.App (S.Ctor c) ps ->
    ck e (IsCtor c $ V.length ps)
    <#> combine (V.imap (\i -> simplifyPat (Field e $ "$" <> showT i)) ps)
  S.Wildcard -> ([], [])
  S.As p v -> simplifyPat e p <#> asg e v

  where
    unroll = foldr (\h t -> S.App (S.Ctor "::") [h, t]) (S.App (S.Ctor "Nil") [])

    ck e' c = ([(e', c)], [])
    asg e' v = ([], [(e', v)])

    combine = foldl' (<#>) ([], [])

    (c1, a1) <#> (c2, a2) = (c1 <> c2, a1 <> a2)
    infixr 6 <#>

simplifyCase :: Ident -> Vector (S.Pat, Expr) -> Expr
simplifyCase v bs = Case $ bs <&> \(p, e) ->
  let (cs, as) = simplifyPat (Var v) p
  in (cs, Let [(False, swap <$> as)] e)

simplifyLam :: MonadReader Int m => Vector S.Pat -> S.Expr -> m Expr
simplifyLam vs e =
  case V.uncons vs of
    Nothing -> simplifyExpr e
    Just (S.Wildcard, ps) -> withVar \v -> Lambda v <$> simplifyLam ps e
    Just (S.Var v, ps) -> Lambda v <$> simplifyLam ps e
    Just (p, ps) -> withVar \v ->
      Lambda v <$> simplifyExpr (S.Case (S.Var v) [(p, S.Lambda ps e)])

freeVars :: Expr -> Set Ident
freeVars NumLit{} = S'.empty
freeVars StrLit{} = S'.empty
freeVars (Record fs) = foldMap freeVars $ V.map snd fs
freeVars (Ctor _ es) = foldMap freeVars es
freeVars (Var v) = S'.singleton v
freeVars (Field e _) = freeVars e
freeVars (Case bs) = foldMap (\(cs, e) -> foldMap (freeVars . fst) cs <> freeVars e) bs
freeVars (Lambda v e) = freeVars e S'.\\ S'.singleton v
freeVars (App f a) = freeVars f <> freeVars a
freeVars (Let bs e) =
  (freeVars e <> foldMap (foldMap (freeVars . snd) . snd) bs)
  S'.\\ foldMap (foldMap (S'.singleton . fst) . snd) bs

depGraph :: Vector (Ident, Expr) -> Vector (Ident, Set Ident)
depGraph bs = V.map (second $ S'.intersection vars . freeVars) bs
  where
    vars = foldMap (S'.singleton . fst) bs

getSCCs :: Vector (Ident, Expr) -> (Map Ident Int, Vector (Bool, Set Ident))
getSCCs bs =
  let
    deps = depGraph bs
    idxs = arrOfVec $ V.map fst deps
    idents = M.fromList $ swap <$> A.assocs idxs
    graph = arrOfVec $ V.map (map (idents M.!) . S'.toList . snd) deps
    sccs = G.scc graph

    arrOfVec v = A.listArray (0, V.length v - 1) $ V.toList v
    setOfTree (Node a ns) = S'.singleton a <> foldMap setOfTree ns
    getIdents t = (isRec, s)
      where
        isRec = S'.size s > 1 || S'.member m (snd $ deps V.! (idents M.! m))
        m = S'.findMin s
        s = S'.map (idxs A.!) $ setOfTree t
  in
    (idents, V.fromList $ getIdents <$> sccs)

simplifyLet :: Vector (Ident, Expr) -> Expr -> Expr
simplifyLet bs = Let (convert <$> sccs)
  where
    (idents, sccs) = getSCCs bs
    convert (r, scc) = (r, V.fromList $ (\i -> bs V.! (idents M.! i)) <$> S'.toList scc)

withVar :: MonadReader Int m => (Ident -> m a) -> m a
withVar f = do
  i <- ask
  local succ $ f $ "_" <> showT i

simplifyExpr :: MonadReader Int m => S.Expr -> m Expr
simplifyExpr (S.NumLit n) = pure $ NumLit n
simplifyExpr (S.StrLit s) = pure $ StrLit s
simplifyExpr (S.Tuple es) = Ctor "Tuple" <$> traverse simplifyExpr es
simplifyExpr (S.List es) =
  foldrM (\h t -> (\h' -> Ctor "::" [h', t]) <$> simplifyExpr h) (Ctor "Nil" []) es
simplifyExpr (S.Record es) = Record <$> for es \case
  (i, Nothing) -> pure (i, Var i)
  (i, Just e) -> (i,) <$> simplifyExpr e
simplifyExpr (S.Var v) = pure $ Var v
simplifyExpr (S.RecordField e f) = (`Field` f) <$> simplifyExpr e
simplifyExpr (S.CtorField e i) = (`Field` ("$" <> showT i)) <$> simplifyExpr e
simplifyExpr (S.Case e bs)
  | S.Var v <- e, T.head v == '_' = simplifyCase v <$> simplifyGroup bs
  | otherwise = withVar \v ->
      (\e' -> Let [(False, [(v, e')])])
      <$> simplifyExpr e
      <*> (simplifyCase v <$> simplifyGroup bs)
simplifyExpr (S.Lambda vs e) = simplifyLam vs e
simplifyExpr (S.LambdaCase bs) = withVar \v -> Lambda v <$> simplifyExpr (S.Case (S.Var v) bs)
simplifyExpr (S.App (S.Ctor ctor) es) = Ctor ctor <$> traverse simplifyExpr es
simplifyExpr (S.App (S.Fn f) es) = foldl' App <$> simplifyExpr f <*> traverse simplifyExpr es
simplifyExpr (S.Let bs e) = simplifyLet <$> simplifyGroup bs <*> simplifyExpr e

simplifyGroup :: (MonadReader Int m, Traversable f, Traversable t) => f (t S.Expr) -> m (f (t Expr))
simplifyGroup = traverse (traverse simplifyExpr)

simplifyExpr' :: S.Expr -> Expr
simplifyExpr' = flip runReader 0 . simplifyExpr

simplify :: S.BindingGroup -> Expr
simplify bs = simplifyExpr' $ S.Let bs $ S.Var "main"
