module Language.Dynasty.Simplify(simplify) where

import Control.Monad.Reader(MonadReader, ask, local, runReader)
import Data.Array qualified as A
import Data.Bifunctor(second)
import Data.Foldable(foldl', foldrM, find)
import Data.Graph(Tree(..))
import Data.Graph qualified as G
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S'
import Data.Text qualified as T
import Data.Traversable(for)
import Data.Tuple(swap)

import Language.Dynasty.Syntax(Ident, Module (..), Import (..))
import Language.Dynasty.Syntax qualified as S
import Language.Dynasty.Core
import Utils(showT, imap)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe, fromJust)

simplifyPat :: Expr -> S.Pat -> ([(Expr, Check)], [(Expr, Ident)])
simplifyPat e = \case
  S.NumLit n -> ck e $ IsNum n
  S.StrLit s -> ck e $ IsStr s
  S.Tuple ps -> simplifyPat e $ S.App (S.Ctor "Tuple") ps
  S.List ps -> simplifyPat e $ unroll ps
  S.Record fs -> combine $ fs <&> \(f, p) ->
    ck e (HasField f) <#> let e' = Field e f in maybe (asg e' f) (simplifyPat e') p
  S.Var v -> asg e v
  S.App (S.Ctor c) ps ->
    ck e (IsCtor c $ length ps)
    <#> combine (imap (\i -> simplifyPat (Field e $ "$" <> showT i)) ps)
  S.Wildcard -> ([], [])
  S.As p v -> simplifyPat e p <#> asg e v

  where
    unroll = foldr (\h t -> S.App (S.Ctor "::") [h, t]) (S.App (S.Ctor "Nil") [])

    ck e' c = ([(e', c)], [])
    asg e' v = ([], [(e', v)])

    combine = foldl' (<#>) ([], [])

    (c1, a1) <#> (c2, a2) = (c1 <> c2, a1 <> a2)
    infixr 6 <#>

simplifyCase :: Ident -> [(S.Pat, Expr)] -> Expr
simplifyCase v bs = Case $ bs <&> \(p, e) ->
  let (cs, as) = simplifyPat (Var v) p
  in (cs, Let [(False, swap <$> as)] e)

simplifyLam :: MonadReader Int m => [S.Pat] -> S.Expr -> m Expr
simplifyLam [] e = simplifyExpr e
simplifyLam (p : ps) e =
  case p of
    S.Wildcard -> withVar \v -> Lambda v <$> simplifyLam ps e
    S.Var v -> Lambda v <$> simplifyLam ps e
    _ -> withVar \v -> Lambda v <$> simplifyExpr (S.Case (S.Var v) [(p, S.Lambda ps e)])

freeVars :: Expr -> Set Ident
freeVars NumLit{} = S'.empty
freeVars StrLit{} = S'.empty
freeVars (Record fs) = foldMap (freeVars . snd) fs
freeVars (Ctor _ es) = foldMap freeVars es
freeVars (Var v) = S'.singleton v
freeVars (Field e _) = freeVars e
freeVars (Case bs) = foldMap (\(cs, e) -> foldMap (freeVars . fst) cs <> freeVars e) bs
freeVars (Lambda v e) = freeVars e S'.\\ S'.singleton v
freeVars (App f a) = freeVars f <> freeVars a
freeVars (Let bs e) =
  (freeVars e <> foldMap (foldMap (freeVars . snd) . snd) bs)
  S'.\\ foldMap (foldMap (S'.singleton . fst) . snd) bs
freeVars (UnsafeJS _ vs _) = S'.fromList vs

depGraph :: [(Ident, Expr)] -> [(Ident, Set Ident)]
depGraph bs = second (S'.intersection vars . freeVars) <$> bs
  where
    vars = foldMap (S'.singleton . fst) bs

getSCCs :: [(Ident, Expr)] -> (Map Ident Int, [(Bool, Set Ident)])
getSCCs bs =
  let
    deps = depGraph bs
    idxs = arrOfList $ fst <$> deps
    idents = M.fromList $ swap <$> A.assocs idxs
    graph = arrOfList $ fmap (idents M.!) . S'.toList . snd <$> deps
    sccs = G.scc graph

    arrOfList l = A.listArray (0, length l - 1) l
    setOfTree (Node a ns) = S'.singleton a <> foldMap setOfTree ns
    getIdents t = (isRec, s)
      where
        isRec = S'.size s > 1 || S'.member m (snd $ deps !! (idents M.! m))
        m = S'.findMin s
        s = S'.map (idxs A.!) $ setOfTree t
  in
    (idents, getIdents <$> sccs)

simplifyLet :: [(Ident, Expr)] -> Expr -> Expr
simplifyLet bs = Let (convert <$> sccs)
  where
    (idents, sccs) = getSCCs bs
    convert (r, scc) = (r, (bs !!) . (idents M.!) <$> S'.toList scc)

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
simplifyExpr (S.Let [] e) = simplifyExpr e
simplifyExpr (S.Let bs e) = simplifyLet <$> simplifyGroup bs <*> simplifyExpr e
simplifyExpr (S.UnsafeJS whnf vs js) = pure $ UnsafeJS whnf vs js
simplifyExpr (S.Do stmts e) = simplifyExpr $ foldr bind' e stmts
  where
    bind' (Nothing, a) e' = S.App (S.Fn $ S.Var ">>=") [a, S.Lambda [S.Wildcard] e']
    bind' (Just i, a) e' = S.App (S.Fn $ S.Var ">>=") [a, S.Lambda [S.Var i] e']

simplifyGroup :: (MonadReader Int m, Traversable f, Traversable t) => f (t S.Expr) -> m (f (t Expr))
simplifyGroup = traverse (traverse simplifyExpr)

modToExpr :: Module -> S.Expr
modToExpr Module{..} =
  S.Let (imports <> moduleBindings) $ S.Record $ (, Nothing) <$> moduleExports
  where
    imports = importBindings =<< moduleImports
    importBindings Import{..} =
      fromMaybe [] importIdents <&> \i -> (i, S.RecordField (S.Var importModule) i)

mainModule :: [Module] -> Ident
mainModule ms = moduleName $ fromJust $ find isMain ms
  where
    isMain Module{..} = "main" `elem` moduleExports

modulesToExpr :: [Module] -> S.Expr
modulesToExpr ms =
  S.Let (toBinding <$> filter ((`elem` imported) . moduleName) ms)
  $ S.RecordField (S.Var mainMod) "main"
  where
    mainMod = mainModule ms
    toBinding m@Module{..} = (moduleName, modToExpr m)
    imported = S'.fromList $ mainMod : ((importModule <$>) . moduleImports =<< ms)

simplify :: [Module] -> Expr
simplify = flip runReader 0 . simplifyExpr . modulesToExpr
