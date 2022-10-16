module Language.Dynasty.Lowering(lower) where

import Control.Monad.Reader(Reader, ask, local, runReader)
import Data.Foldable(foldl', foldrM, foldMap')
import Data.Functor((<&>))
import Data.Graph(SCC(..))
import Data.Graph qualified as G
import Data.Maybe(fromMaybe)
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Traversable(for)

import Language.Dynasty.Core qualified as C
import Language.Dynasty.Syntax
import Language.Dynasty.Utils(imap, showT)

type VarGen = Reader Int

lowerPat :: C.Expr -> Pat -> ([(C.Expr, C.Check)], [(C.Expr, Id)])
lowerPat e = \case
  NumPat n -> ck e $ C.IsNum n
  StrPat s -> ck e $ C.IsStr s
  TupPat ps -> lowerPat e $ CtorPat "Tuple" ps
  ListPat ps -> lowerPat e $ unroll ps
  RecPat fs -> combine $ fs <&> \(f, p) ->
    ck e (C.HasField f) <#> let e' = C.Field e f in maybe (asg e' f) (lowerPat e') p
  VarPat v -> asg e v
  CtorPat c ps ->
    ck e (C.IsCtor c $ length ps)
    <#> combine (imap (\i -> lowerPat (C.Field e $ "$" <> showT i)) ps)
  Wildcard -> ([], [])
  As v p -> lowerPat e p <#> asg e v

  where
    unroll = foldr (\h t -> CtorPat "::" [h, t]) $ CtorPat "Nil" []

    ck e' c = ([(e', c)], [])
    asg e' v = ([], [(e', v)])

    combine = foldl' (<#>) ([], [])

    (c1, a1) <#> (c2, a2) = (c1 <> c2, a1 <> a2)
    infixr 6 <#>

lowerCase :: Id -> [(Pat, C.Expr)] -> C.Expr
lowerCase v bs = C.Case $ bs <&> \(p, e) ->
  lowerPat (C.Var $ Unqual v) p <&> \case
    [] -> e
    as -> foldr (\(e, i) -> C.Let i e) e as

lowerLam :: [Pat] -> Expr -> VarGen C.Expr
lowerLam [] e = lowerExpr e
lowerLam (p : ps) e =
  case p of
    Wildcard -> withFreshVar \v -> C.Lam v <$> lowerLam ps e
    VarPat v -> C.Lam v <$> lowerLam ps e
    _ -> withFreshVar \v -> C.Lam v <$> lowerExpr (Case (Var $ Unqual v) [(p, Lam ps e)])

freeVars :: C.Expr -> Set Id
freeVars C.NumLit{} = S.empty
freeVars C.StrLit{} = S.empty
freeVars (C.RecLit fs) = foldMap' (freeVars . snd) fs
freeVars (C.CtorLit _ es) = foldMap' freeVars es
freeVars (C.Var (Unqual v)) = S.singleton v
freeVars (C.Var Qual{}) = S.empty
freeVars (C.Field e _) = freeVars e
freeVars (C.Case bs) = foldMap' (\(cs, e) -> foldMap' (freeVars . fst) cs <> freeVars e) bs
freeVars (C.Lam v e) = freeVars e S.\\ S.singleton v
freeVars (C.App f a) = freeVars f <> freeVars a
freeVars (C.Let i v e) = S.delete i $ freeVars v <> freeVars e
freeVars (C.LetRec bs e) =
  (freeVars e <> foldMap' (foldMap' freeVars) bs)
  S.\\ foldMap' (S.singleton . fst) bs
freeVars (C.UnsafeJS vs _) = S.fromList vs

splitRecGroups :: [(Id, C.Expr)] -> [SCC (Id, C.Expr)]
splitRecGroups bs = G.stronglyConnComp deps
  where
    vars = S.fromList $ fst <$> bs
    deps = bs <&> \b@(i, e) -> (b, i, S.toList $ S.intersection vars $ freeVars e)

lowerBindingGroup :: BindingGroup -> VarGen [SCC (Id, C.Expr)]
lowerBindingGroup bs = splitRecGroups <$> traverse (traverse lowerExpr) bs

simplifyLet :: Set Id -> C.Expr -> [SCC (Id, C.Expr)] -> C.Expr
simplifyLet _ e [] = e
simplifyLet f e (AcyclicSCC (i, v) : bs) | S.member i f =
  simplifyLet (freeVars v <> S.delete i f) (C.Let i v e) bs
simplifyLet f e (CyclicSCC b : bs) | any (`S.member` f) is =
  simplifyLet (foldMap' freeVars (snd <$> b) <> (f S.\\ is)) (C.LetRec b e) bs
  where
    is = S.fromList $ fst <$> b
simplifyLet f e (_ : bs) = simplifyLet f e bs

lowerLet :: BindingGroup -> Expr -> VarGen C.Expr
lowerLet bs e =
  (\bs e -> simplifyLet (freeVars e) e $ reverse bs) <$> lowerBindingGroup bs <*> lowerExpr e

withFreshVar :: (Id -> VarGen a) -> VarGen a
withFreshVar f = do
  i <- ask
  local succ $ f $ "_" <> showT i

lowerExpr :: Expr -> VarGen C.Expr
lowerExpr (NumLit n) = pure $ C.NumLit n
lowerExpr (StrLit s) = pure $ C.StrLit s
lowerExpr (TupLit es) = C.CtorLit "Tuple" <$> traverse lowerExpr es
lowerExpr (ListLit es) =
  foldrM (\h t -> (\h' -> C.CtorLit "::" [h', t]) <$> lowerExpr h) (C.CtorLit "Nil" []) es
lowerExpr (RecLit es) = C.RecLit <$> for es \case
  (i, Nothing) -> pure (i, C.Var $ Unqual i)
  (i, Just e) -> (i,) <$> lowerExpr e
lowerExpr (Var v) = pure $ C.Var v
lowerExpr (RecField e f) = (`C.Field` f) <$> lowerExpr e
lowerExpr (CtorField e i) = (`C.Field` ("$" <> showT i)) <$> lowerExpr e
lowerExpr (Case e bs)
  | Var (Unqual v) <- e, T.head v == '_' = lowerCase v <$> traverse (traverse lowerExpr) bs
  | otherwise = withFreshVar \v ->
      C.Let v <$> lowerExpr e <*> (lowerCase v <$> traverse (traverse lowerExpr) bs)
lowerExpr (Lam vs e) = lowerLam vs e
lowerExpr (LamCase bs) = withFreshVar \v -> C.Lam v <$> lowerExpr (Case (Var $ Unqual v) bs)
lowerExpr (CtorLit ctor es) = C.CtorLit ctor <$> traverse lowerExpr es
lowerExpr (App f a) = C.App <$> lowerExpr f <*> lowerExpr a
lowerExpr (Let [] e) = lowerExpr e
lowerExpr (Let bs e) = lowerLet bs e
lowerExpr (UnsafeJS free js) = pure $ C.UnsafeJS free js
lowerExpr (Do n stmts e) = lowerExpr $ foldr bind' e stmts
  where
    bind' (p, a) e' = (Var n `App` a) `App` Lam [fromMaybe Wildcard p] e'

lowerModule :: Module -> C.Module
lowerModule m =
  C.Module m.name
  $ lowerExpr (Let m.bindings $ RecLit $ (, Nothing) <$> m.exports) `runReader` 0

lower :: Program -> ([C.Module], Id)
lower p =
  ( lowerModule <$> p.modules
  , p.main
  )
