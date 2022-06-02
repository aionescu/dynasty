module Language.Dynasty.Simplify(simplify) where

import Control.Monad.Reader(MonadReader, asks, local, runReader)
import Data.Array qualified as A
import Data.Bifunctor(second)
import Data.Foldable(foldrM)
import Data.Function((&))
import Data.Graph(Tree(..))
import Data.Graph qualified as G
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S'
import Data.Traversable(for)
import Data.Tuple(swap)
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Core
import Language.Dynasty.Syntax qualified as S
import Utils

patDig :: S.Pat -> Dig
patDig (S.Lit l) = Check $ IsLit l
patDig (S.Tuple ps) = patDig $ S.App (S.Ctor "Tuple") ps

patDig (S.List ps) = patDig $
  V.foldr (\h t -> S.App (S.Ctor "Cons") [h, t]) (S.App (S.Ctor "Nil") []) ps

patDig (S.Record fs) =
  fs
  & V.map (\(i, p) -> RecField i $ maybe (Assign i) patDig p)
  & V.foldl' And (Check IsRecord)

patDig (S.App (S.Ctor ctor) ps) =
  ps
  & V.imap (\i p -> Field i $ patDig p)
  & V.foldl' And (Check $ IsCtor ctor)

patDig (S.Var v) = Assign v
patDig S.Wildcard = Check NoOp
patDig (S.OfType v p) = And (TypeOf $ patDig p) $ Assign v
patDig (S.As v p) = And (patDig p) $ Assign v

simplifyCaseBranch :: MonadReader Int m => S.Pat -> S.Expr -> m (Dig, Expr)
simplifyCaseBranch p e = (patDig p,) <$> simplifyExpr e

withVar :: MonadReader Int m => (Ident -> m a) -> m a
withVar f = do
  asks (\i -> "$" <> showT i) >>= local succ . f

simplifyLam :: MonadReader Int m => Vector S.Pat -> S.Expr -> m Expr
simplifyLam vs e =
  case V.uncons vs of
    Nothing ->  simplifyExpr e
    Just (S.Var v, ps) -> Lambda v <$> simplifyLam ps e
    Just (p, ps) -> withVar \v -> Lambda v <$> simplifyExpr (S.Case (S.Var v) [(p, S.Lambda ps e)])

digVars :: Dig -> Set Ident
digVars (Field _ d) = digVars d
digVars (RecField _ d) = digVars d
digVars (TypeOf d) = digVars d
digVars (And a b) = digVars a <> digVars b
digVars Check{} = S'.empty
digVars (Assign v) = S'.singleton v

freeVars :: Expr -> Set Ident
freeVars Lit{} = S'.empty
freeVars (Record fs) = foldMap freeVars $ V.map snd fs
freeVars (Ctor _ es) = foldMap freeVars es
freeVars (Var v) = S'.singleton v
freeVars (FieldAccess e _) = freeVars e
freeVars (Case v bs) = S'.singleton v <> foldMap (\(d, b) -> freeVars b S'.\\ digVars d) bs
freeVars (Lambda v e) = freeVars e S'.\\ S'.singleton v
freeVars (App f a) = freeVars f <> freeVars a
freeVars (Let v e e') = freeVars e <> (freeVars e' S'.\\ S'.singleton v)
freeVars (LetRec bs e) = (freeVars e <> foldMap (freeVars . snd) bs) S'.\\ foldMap (S'.singleton . fst) bs

depGraph :: S.BindingGroup -> Vector (Ident, Set Ident)
depGraph bs = V.map (second $ S'.intersection vars . freeVars . simplifyExpr') bs
  where
    vars = foldMap (S'.singleton . fst) bs

getSCCs :: S.BindingGroup -> (Map Ident Int, Vector (Bool, Set Ident))
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

simplifyLet :: forall m. MonadReader Int m => Vector (Ident, S.Expr) -> S.Expr -> m Expr
simplifyLet bs e =
  let
    (idents, sccs) = getSCCs bs
    convert (r, scc) = (r, V.fromList $ (\i -> bs V.! (idents M.! i)) <$> S'.toList scc)

    mkLet (r, is) e'
      | r = LetRec <$> is' <*> pure e'
      | otherwise = Let i <$> simplifyExpr v <*> pure e'
      where
        (i, v) = V.head is
        is' = traverse (traverse simplifyExpr) is
  in
    flip (foldrM mkLet) (convert <$> sccs) =<< simplifyExpr e

simplifyExpr :: MonadReader Int m => S.Expr -> m Expr
simplifyExpr (S.Lit l) = pure $ Lit l
simplifyExpr (S.Tuple es) = Ctor "Tuple" <$> traverse simplifyExpr es

simplifyExpr (S.List es) =
  foldrM
  (\e t -> (\h -> Ctor "Const" [h, t]) <$> simplifyExpr e)
  (Ctor "Nil" [])
  es

simplifyExpr (S.Record es) = Record <$> for es \case
  (i, Nothing) -> pure (i, Var i)
  (i, Just e) -> (i,) <$> simplifyExpr e

simplifyExpr (S.Var v) = pure $ Var v
simplifyExpr (S.FieldAccess e f) = (`FieldAccess` f) <$> simplifyExpr e

simplifyExpr (S.Case (S.Var v) bs) = Case v <$> traverse (uncurry simplifyCaseBranch) bs
simplifyExpr (S.Case e bs) = withVar \v ->
  (Let v <$> simplifyExpr e) <*> (Case v <$> traverse (uncurry simplifyCaseBranch) bs)

simplifyExpr (S.Lambda vs e) = simplifyLam vs e
simplifyExpr (S.LambdaCase bs) = withVar \v -> Lambda v <$> simplifyExpr (S.Case (S.Var v) bs)
simplifyExpr (S.App (S.Ctor ctor) es) = Ctor ctor <$> traverse simplifyExpr es
simplifyExpr (S.App (S.Fn f) es) = V.foldl' App <$> simplifyExpr f <*> traverse simplifyExpr es
simplifyExpr (S.Let bs e) = simplifyLet bs e

simplifyExpr' :: S.Expr -> Expr
simplifyExpr' = flip runReader 0 . simplifyExpr

simplify :: S.BindingGroup -> Expr
simplify bs = simplifyExpr' $ S.Let bs $ S.Var "main"
