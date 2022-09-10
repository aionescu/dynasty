{-# LANGUAGE StrictData #-}

module Language.Dynasty.NameResolution(resolveNames) where

import Control.Monad(join)
import Control.Monad.Reader(ReaderT(runReaderT), local, ask)
import Control.Monad.State(StateT, gets, modify, execStateT)
import Control.Monad.Trans.Class(lift)
import Data.Bifunctor(first)
import Data.Foldable(foldMap', traverse_, find)
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)

import Language.Dynasty.Syntax
import Language.Dynasty.Utils(err, findDuplicate, showT)

data Env =
  Env
  { vars :: Map Id Name
  , imports :: [Id]
  , exports :: Map Id (Set Id)
  }

type Reso = ReaderT Env (Either Text)
type Valid = StateT (Set Id) (Either Text)

addVar :: Id -> Valid ()
addVar v = gets (S.member v) >>= \case
  True -> err $ "Duplicate identifier " <> showT v <> " in pattern"
  False -> modify (S.insert v)

validatePat :: Pat -> Valid ()
validatePat NumPat{} = pure ()
validatePat StrPat{} = pure ()
validatePat (TupPat es) = traverse_ validatePat es
validatePat (ListPat es) = traverse_ validatePat es
validatePat (RecPat es) =
  case findDuplicate $ fst <$> es of
    Just f -> err $ "Duplicate record field " <> f
    Nothing -> traverse_ (\(f, e) -> maybe (addVar f) validatePat e) es
validatePat (VarPat v) = addVar v
validatePat (CtorPat _ es) = traverse_ validatePat es
validatePat Wildcard = pure ()
validatePat (As v p) = validatePat p *> addVar v

patVars :: Pat -> Reso [Id]
patVars pat = lift $ S.toList <$> execStateT (validatePat pat) S.empty

withPatVars :: Pat -> Reso a -> Reso a
withPatVars p m = do
  vars <- patVars p
  withVars vars m

resolveBranch :: (Pat, Expr) -> Reso (Pat, Expr)
resolveBranch (p, e) = withPatVars p $ (p,) <$> resolveExpr e

resolveDo :: Name -> [(Maybe Pat, Expr)] -> Expr -> Reso Expr
resolveDo n = go []
  where
    go acc [] e = Do n (reverse acc) <$> resolveExpr e
    go acc ((b, s) : ss) e =
      resolveExpr s >>= \s -> maybe id withPatVars b $ go ((b, s) : acc) ss e

resolveExpr :: Expr -> Reso Expr
resolveExpr e@NumLit{} = pure e
resolveExpr e@StrLit{} = pure e
resolveExpr (TupLit es) = TupLit <$> traverse resolveExpr es
resolveExpr (ListLit es) = ListLit <$> traverse resolveExpr es
resolveExpr (RecLit fs) =
  case findDuplicate $ fst <$> fs of
    Just f -> err $ "Duplicate record field " <> f
    Nothing -> RecLit <$> traverse (traverse (traverse resolveExpr)) fs
resolveExpr (CtorLit c es) = CtorLit c <$> traverse resolveExpr es
resolveExpr (Var (Unqual v)) = ask >>= \Env{..} ->
  case vars M.!? v of
    Nothing -> err $ "Unbound identifier " <> v
    Just n -> pure $ Var n
resolveExpr e@(Var (Qual m v)) = ask >>= \Env{..} ->
  if m `notElem` imports
  then err $ "Qualified use of un-imported module " <> m
  else if S.notMember v (exports M.! m)
  then err $ "Module " <> m <> " does not export the identifier " <> v
  else pure e
resolveExpr (RecField e f) = (`RecField` f) <$> resolveExpr e
resolveExpr (CtorField e f) = (`CtorField` f) <$> resolveExpr e
resolveExpr (Case e bs) = Case <$> resolveExpr e <*> traverse resolveBranch bs
resolveExpr (Lam ps e) =
  traverse patVars ps >>= \vs -> withVars (join vs) $ Lam ps <$> resolveExpr e
resolveExpr (LamCase bs) = LamCase <$> traverse resolveBranch bs
resolveExpr (App f a) = App <$> resolveExpr f <*> resolveExpr a
resolveExpr (Let bs e) =
  resolveBindingGroup bs >>= \bs ->
    withVars (fst <$> bs) $ Let bs <$> resolveExpr e
resolveExpr (Do _ ss e) = ask >>= \Env{..} ->
  case vars M.!? ">>=" of
    Nothing -> err "do-expressions require (>>=) to be in scope"
    Just n -> resolveDo n ss e
resolveExpr e@UnsafeJS{} = pure e

withVars :: [Id] -> Reso a -> Reso a
withVars vs = local (\e@Env{..} -> e{vars = M.fromList ((\i -> (i, Unqual i)) <$> vs) <> vars})

resolveBindingGroup :: BindingGroup -> Reso BindingGroup
resolveBindingGroup bs =
  let vars = fst <$> bs in
  case findDuplicate vars of
    Just i -> err $ "Duplicate definition of " <> i <> " in the same binding group"
    Nothing -> withVars vars $ traverse (traverse resolveExpr) bs

checkImports :: Map Id (Set Id) -> [Id] -> Reso ()
checkImports exports imports =
  case find (`M.notMember` exports) imports of
    Nothing -> pure ()
    Just m -> err $ "Import of inexistent module " <> m

resolveModule :: Map Id (Set Id) -> Module -> Either Text Module
resolveModule exports m@Module{..} =
  first (("In module " <> moduleName <> ": ") <>)
  $ (\bg -> m{moduleBindings = bg})
  <$> runReaderT
    (checkImports exports moduleImports *> resolveBindingGroup moduleBindings)
    (Env (foldMap' exported moduleImports) moduleImports exports)
  where
    exported m = M.fromSet (Qual m) $ exports M.! m

resolveNames :: Program -> Either Text Program
resolveNames p@Program{..} =
  (\ms -> p{programModules = ms})
  <$> traverse (resolveModule exports) programModules
  where
    exports =
      M.fromList
      $ (\Module{..} -> (moduleName, S.fromList $ fst <$> moduleBindings))
        <$> programModules
