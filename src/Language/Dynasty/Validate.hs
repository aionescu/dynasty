module Language.Dynasty.Validate(validate) where

import Control.Monad.Except(MonadError, throwError, liftEither, mapError)
import Control.Monad.Reader(MonadReader, asks, local, ReaderT(runReaderT), ask)
import Control.Monad.State.Strict(MonadState, gets, modify, execStateT)
import Data.Foldable(traverse_)
import Data.Function((&))
import Data.Functor(($>))
import Data.Map.Strict qualified as M
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)

import Language.Dynasty.Syntax
import Utils
import Data.Containers.ListUtils (nubOrdOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad (unless, when)
import Data.Bifunctor (first)

uniqueBy :: Ord b => (a -> b) -> [a] -> Bool
uniqueBy f l = length l == length (nubOrdOn f l)

uniqueIdents :: [(Ident, a)] -> Bool
uniqueIdents = uniqueBy fst

type Env = Set Ident

addVar :: (MonadError Text m, MonadState Env m) => Ident -> m ()
addVar v = gets (S.member v) >>= \case
  True -> throwError $ "Duplicate variable " <> showT v <> " in pattern"
  False -> modify (S.insert v)

validatePat :: (MonadError Text m, MonadState Env m) => Pat -> m ()
validatePat NumLit{} = pure ()
validatePat StrLit{} = pure ()
validatePat (Tuple es) = traverse_ validatePat es
validatePat (List es) = traverse_ validatePat es

validatePat (Record es)
  | not $ uniqueIdents es = throwError "Duplicate field in record pattern"
  | otherwise = traverse_ (\(f, e) -> maybe (addVar f) validatePat e) es

validatePat (Var v) = addVar v
validatePat (App (Ctor _) es) = traverse_ validatePat es
validatePat Wildcard = pure ()
validatePat (As p v) = validatePat p *> addVar v

patVars :: MonadError Text m => Pat -> m Env
patVars p = liftEither $ execStateT (validatePat p) S.empty

withPatVars :: (MonadError Text m, MonadReader Env m) => Pat -> m a -> m a
withPatVars p m = do
  vars <- patVars p
  local (<> vars) m

validateBranch :: (MonadError Text m, MonadReader Env m) => Pat -> Expr -> m ()
validateBranch p e = withPatVars p $ validateExpr e

validateDo :: (MonadError Text m, MonadReader Env m) => [(Maybe Ident, Expr)] -> Expr -> m ()
validateDo [] e = validateExpr e
validateDo ((Nothing, e) : es) e' = validateExpr e *> validateDo es e'
validateDo ((Just v, e) : es) e' = validateExpr e *> withVars (S.singleton v) (validateDo es e')

withVars :: (MonadError Text m, MonadReader Env m) => Env -> m a -> m a
withVars vs = local (<> vs)

validateExpr :: (MonadError Text m, MonadReader Env m) => Expr -> m ()
validateExpr NumLit{} = pure ()
validateExpr StrLit{} = pure ()
validateExpr (Tuple es) = traverse_ validateExpr es
validateExpr (List es) = traverse_ validateExpr es
validateExpr (Record es)
  | not $ uniqueIdents es = throwError "Duplicate field in record expression"
  | otherwise = traverse_ validateExpr $ mapMaybe snd es
validateExpr (Var v) = asks (S.member v) >>= \case
  False -> throwError $ "Undefined variable " <> showT v
  True -> pure ()
validateExpr (RecordField e _) = validateExpr e
validateExpr (CtorField e _) = validateExpr e
validateExpr (Case s bs) = validateExpr s *> traverse_ (uncurry validateBranch) bs
validateExpr (Lambda vs e) = traverse patVars vs >>= \vars -> local (<> S.unions vars) $ validateExpr e
validateExpr (LambdaCase bs) = traverse_ (uncurry validateBranch) bs
validateExpr (App (Ctor _) es) = traverse_ validateExpr es
validateExpr (App (Fn f) es) = validateExpr f *> traverse_ validateExpr es
validateExpr (Let bs e) = withBindingGroup bs $ validateExpr e
validateExpr (Do stmts e) = validateDo stmts e
validateExpr (UnsafeJS _ vs _) =
  ask >>= \env ->
    unless (all (`elem` env) vs) do
      throwError "UnsafeJS: Undefined variable"

withBindingGroup :: (MonadReader Env m, MonadError Text m) => BindingGroup -> m () -> m ()
withBindingGroup bs m
  | not $ uniqueIdents bs = throwError "Variable redefined in binding group"
  | otherwise = withVars (S.fromList $ fst <$> bs) $ traverse_ (validateExpr . snd) bs *> m

validateBindingGroup :: (MonadReader Env m, MonadError Text m) => BindingGroup -> m ()
validateBindingGroup bs = withBindingGroup bs $ pure ()

mainExists :: MonadError Text m => [Module] -> m ()
mainExists ms =
  case filter isMain ms of
    [_] -> pure ()
    [] -> throwError "No suitable main module found"
    _ -> throwError "Multiple suitable main modules found"
  where
    isMain Module{..} = "main" `elem` moduleExports

modulesUnique :: MonadError Text m => [Module] -> m ()
modulesUnique ms =
  unless (uniqueBy moduleName ms) do
    throwError "Duplicate module name declaration"

importsUnique :: MonadError Text m => Module -> m ()
importsUnique Module{..} =
  unless (uniqueBy importModule moduleImports) do
    throwError "Duplicate import"

importIdentsUnique :: MonadError Text m => Module -> m ()
importIdentsUnique Module{..} =
  unless (uniqueBy id (importIdents <$> moduleImports)) do
    throwError "Identifier imported multiple times"

importsNotShadowed :: MonadError Text m => Module -> m ()
importsNotShadowed Module{..} =
  when (any (`elem` imports) $ fst <$> moduleBindings) do
    throwError "Imported identifier is shadowed"
  where
    imports = fromMaybe [] . importIdents =<< moduleImports

exportsUnique :: MonadError Text m => Module -> m ()
exportsUnique Module{..} =
  unless (uniqueBy id moduleExports) do
    throwError "Identifier exported multiple times"

exportsDefined :: MonadError Text m => Module -> m ()
exportsDefined Module{..} =
  unless (all (`elem` idents) moduleExports) do
    throwError "Undeclared identifier exported"
  where
    idents = fst <$> moduleBindings

validateModule :: (MonadReader Env m, MonadError Text m) => Module -> m ()
validateModule m@Module{..} =
  mapError (first (("In module " <> moduleName <> ": ") <>) <$>) do
    importsUnique m
    importIdentsUnique m
    importsNotShadowed m
    exportsUnique m
    exportsDefined m
    withVars (S.fromList $ fromMaybe [] . importIdents =<< moduleImports) $
      validateBindingGroup moduleBindings

validateModules :: (MonadReader Env m, MonadError Text m) => [Module] -> m ()
validateModules ms = do
  modulesUnique ms
  mainExists ms
  traverse_ validateModule ms

fillImports :: [Module] -> [Module]
fillImports ms = go <$> ms
  where
    go m@Module{..} = m { moduleImports = fill <$> moduleImports }
    fill i@Import{..} =
      i { importIdents = Just $ fromMaybe (modExports M.! importModule) importIdents }
    modExports = M.fromList $ ((,) <$> moduleName <*> moduleExports) <$> ms

validate :: [Module] -> Either Text [Module]
validate (fillImports -> ms) =
  validateModules ms $> ms
  & flip runReaderT S.empty
  & first ("Error: " <>)
