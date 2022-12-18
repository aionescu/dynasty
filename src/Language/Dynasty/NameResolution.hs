{-# LANGUAGE StrictData #-}

module Language.Dynasty.NameResolution(resolveNames) where

import Control.Monad(join)
import Control.Monad.Except(throwError, liftEither, withError)
import Control.Monad.Reader(ReaderT(runReaderT), local, ask, asks)
import Control.Monad.State(StateT, gets, modify, execStateT)
import Control.Monad.Trans.Writer.CPS(WriterT, tell, runWriterT)
import Data.Foldable(foldMap', traverse_, find)
import Data.Function((&))
import Data.Functor((<&>), ($>))
import Data.Map.Monoidal.Strict(MonoidalMap)
import Data.Map.Monoidal.Strict qualified as MM
import Data.Map.Strict(Map)
import Data.Map.Strict qualified as M
import Data.Maybe(mapMaybe)
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)

import Language.Dynasty.Syntax
import Language.Dynasty.Utils(findDup, showT)

data Env =
  Env
  { envVars :: Map Id Name
  , envImports :: [Id]
  , envExports :: Map Id (Set Id)
  }

type Exports = MonoidalMap Id (Set Id)

type Reso = WriterT Exports (ReaderT Env (Either Text))

tellExport :: Name -> Reso ()
tellExport Unqual{} = pure ()
tellExport (Qual m i) = tell $ MM.singleton m $ S.singleton i

type Valid = StateT (Set Id) (Either Text)

addVar :: Id -> Valid ()
addVar v = gets (S.member v) >>= \case
  True -> throwError $ "Duplicate identifier " <> showT v <> " in pattern"
  False -> modify (S.insert v)

validatePat :: Pat -> Valid ()
validatePat NumPat{} = pure ()
validatePat StrPat{} = pure ()
validatePat (TupPat es) = traverse_ validatePat es
validatePat (ListPat es) = traverse_ validatePat es
validatePat (RecPat es) = do
  findDup (fst <$> es) ("Duplicate record field " <>)
  traverse_ (\(f, e) -> maybe (addVar f) validatePat e) es
validatePat (VarPat v) = addVar v
validatePat (CtorPat _ es) = traverse_ validatePat es
validatePat Wildcard = pure ()
validatePat (As v p) = validatePat p *> addVar v

patVars :: Pat -> Reso [Id]
patVars pat = liftEither $ S.toList <$> execStateT (validatePat pat) S.empty

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
resolveExpr (RecLit fs) = do
  findDup (fst <$> fs) ("Duplicate record field " <>)
  RecLit <$> traverse (traverse (traverse resolveExpr)) fs
resolveExpr (CtorLit c es) = CtorLit c <$> traverse resolveExpr es
resolveExpr (Var (Unqual v)) = ask >>= \Env{..} ->
  case envVars M.!? v of
    Nothing -> throwError $ "Unbound identifier " <> v
    Just n -> tellExport n $> Var n
resolveExpr e@(Var n@(Qual m v)) = ask >>= \Env{..} -> if
  | m `notElem` envImports -> throwError $ "Qualified use of un-imported module " <> m
  | S.notMember v (envExports M.! m) -> throwError $ "Module " <> m <> " does not export the identifier " <> v
  | otherwise -> tellExport n $> e
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
  case envVars M.!? ">>=" of
    Nothing -> throwError "do-expressions require (>>=) to be in scope"
    Just n -> tellExport n *> resolveDo n ss e
resolveExpr e@UnsafeJS{} = pure e

withVars :: [Id] -> Reso a -> Reso a
withVars vs = local (\e@Env{..} -> e{envVars = M.fromList ((\i -> (i, Unqual i)) <$> vs) <> envVars})

resolveBindingGroup :: BindingGroup -> Reso BindingGroup
resolveBindingGroup bs = do
  let vars = fst <$> bs
  findDup vars \i -> "Duplicate definition of " <> i <> " in the same binding group"
  withVars vars $ traverse (traverse resolveExpr) bs

checkImports :: [Id] -> Reso ()
checkImports imports = ask >>= \Env{..} ->
  case find (`M.notMember` envExports) imports of
    Nothing -> pure ()
    Just m -> throwError $ "Import of inexistent module " <> m

resolveModule :: Module -> Reso Module
resolveModule m = do
  exports <- asks (.envExports)
  let toQual m = M.fromSet (Qual m) $ exports M.! m

  local (\e -> e{envVars = foldMap' toQual m.imports, envImports = m.imports}) do
    withError (("In module " <> m.name <> ": ") <>) do
      checkImports m.imports
      bindings <- resolveBindingGroup m.bindings
      pure m{bindings}

resolveNames :: Program -> Either Text Program
resolveNames p =
  tellExport (Qual p.main "main")
  *> traverse resolveModule p.modules
  & runWriterT
  & flip runReaderT (Env M.empty [] exports)
  <&> \(ms, used) -> p{modules = mapMaybe (keepUsed used) ms}
  where
    keepUsed used m =
      used MM.!? m.name <&> \used -> m{exports = S.toList used}

    exports =
      M.fromList
      $ (\m -> (m.name, S.fromList $ fst <$> m.bindings))
      <$> p.modules
