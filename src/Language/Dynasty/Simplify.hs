module Language.Dynasty.Simplify(simplify) where

import Data.Array qualified as A
import Data.Bifunctor(bimap, second)
import Data.Foldable(foldl')
import Data.Function((&))
import Data.Functor((<&>))
import Data.Graph(Tree(..))
import Data.Graph qualified as G
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S'
import Data.Tuple(swap)
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident)
import Language.Dynasty.Syntax qualified as S
import Language.Dynasty.Core

-- This module converts the `Syntax` AST into `Core`.
-- The main differences between Syntax and Core are:
-- * Core differentiates between Let and LetRec.
-- * The are no patterns in Core. Instead, `Case` stores a `Dig` for each branch, which is a
--   deconstruction of a pattern into its "checks" and "assignments".
-- * Lambdas have exactly one identifier argument in Core. Lambdas with pattern arguments and
--   LambdaCase are simplified into a series of Lambdas and Cases.

patDig :: S.Pat -> Dig
patDig (S.NumLit n) = Check $ IsNum n
patDig (S.StrLit s) = Check $ IsStr s
patDig (S.Tuple ps) = patDig $ S.App (S.Ctor "Tuple") ps

patDig (S.List ps) = patDig $
  V.foldr (\h t -> S.App (S.Ctor "::") [h, t]) (S.App (S.Ctor "Nil") []) ps

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
patDig (S.As p v) = And (patDig p) $ Assign v

simplifyLam :: Vector S.Pat -> S.Expr -> Expr
simplifyLam vs e =
  case V.uncons vs of
    Nothing -> simplifyExpr e
    Just (S.Wildcard, ps) -> Lambda "_" $ simplifyLam ps e
    Just (S.Var v, ps) -> Lambda v $ simplifyLam ps e
    Just (p, ps) -> Lambda "_" $ simplifyExpr $ S.Case (S.Var "_") [(p, S.Lambda ps e)]

digVars :: Dig -> Set Ident
digVars (Field _ d) = digVars d
digVars (RecField _ d) = digVars d
digVars (And a b) = digVars a <> digVars b
digVars Check{} = S'.empty
digVars (Assign v) = S'.singleton v

freeVars :: Expr -> Set Ident
freeVars NumLit{} = S'.empty
freeVars StrLit{} = S'.empty
freeVars (Record fs) = foldMap freeVars $ V.map snd fs
freeVars (Ctor _ es) = foldMap freeVars es
freeVars (Var v) = S'.singleton v
freeVars (FieldAccess e _) = freeVars e
freeVars (Case v bs) = S'.singleton v <> foldMap (\(d, b) -> freeVars b S'.\\ digVars d) bs
freeVars (Lambda v e) = freeVars e S'.\\ S'.singleton v
freeVars (App f a) = freeVars f <> freeVars a
freeVars (Let v e e') = freeVars e <> (freeVars e' S'.\\ S'.singleton v)
freeVars (LetRec bs e) =
  (freeVars e <> foldMap (freeVars . snd) bs) S'.\\ foldMap (S'.singleton . fst) bs

depGraph :: S.BindingGroup -> Vector (Ident, Set Ident)
depGraph bs = V.map (second $ S'.intersection vars . freeVars . simplifyExpr) bs
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

simplifyLet :: Vector (Ident, S.Expr) -> S.Expr -> Expr
simplifyLet bs expr =
  let
    (idents, sccs) = getSCCs bs
    convert (r, scc) = (r, V.fromList $ (\i -> bs V.! (idents M.! i)) <$> S'.toList scc)

    mkLet (r, is) e
      | r = LetRec (second simplifyExpr <$> is) e
      | otherwise = Let i (simplifyExpr v) e
      where
        (i, v) = V.head is
  in
    foldr mkLet (simplifyExpr expr) $ convert <$> sccs

simplifyExpr :: S.Expr -> Expr
simplifyExpr (S.NumLit n) = NumLit n
simplifyExpr (S.StrLit s) = StrLit s
simplifyExpr (S.Tuple es) = Ctor "Tuple" $ simplifyExpr <$> es
simplifyExpr (S.List es) = foldr (\h t -> Ctor "::" [simplifyExpr h, t]) (Ctor "Nil" []) es

simplifyExpr (S.Record es) = Record $ es <&> \case
  (i, Nothing) -> (i, Var i)
  (i, Just e) -> (i, simplifyExpr e)

simplifyExpr (S.Var v) = Var v
simplifyExpr (S.FieldAccess e f) = FieldAccess (simplifyExpr e) f
simplifyExpr (S.Case (S.Var v) bs) = Case v $ bimap patDig simplifyExpr <$> bs
simplifyExpr (S.Case e bs) =
  Let "_" (simplifyExpr e) $ Case "_" $ bimap patDig simplifyExpr <$> bs
simplifyExpr (S.Lambda vs e) = simplifyLam vs e
simplifyExpr (S.LambdaCase bs) = Lambda "_" $ simplifyExpr $ S.Case (S.Var "_") bs
simplifyExpr (S.App (S.Ctor ctor) es) = Ctor ctor $ simplifyExpr <$> es
simplifyExpr (S.App (S.Fn f) es) = foldl' App (simplifyExpr f) $ simplifyExpr <$> es
simplifyExpr (S.Let bs e) = simplifyLet bs e

simplify :: S.BindingGroup -> Expr
simplify bs = simplifyExpr $ S.Let bs $ S.Var "main"
