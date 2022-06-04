module Language.Dynasty.Validate(validate) where

import Control.Monad.Except(MonadError, throwError, liftEither)
import Control.Monad.Reader(MonadReader, asks, local, ReaderT (runReaderT))
import Control.Monad.State.Strict(MonadState, gets, modify, execStateT)
import Data.Foldable(traverse_, fold)
import Data.Function((&))
import Data.Functor(($>))
import Data.Set(Set)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax
import Utils

-- This module validates the Syntax before it is simplified into Core.
-- Currently it checks for:
-- * Undefined variables
-- * Duplicate record fields
-- * Duplicate variables within the same pattern
-- * Duplicate bindings in let expressions

uniqueIdents :: Vector (Ident, a) -> Bool
uniqueIdents v = V.length (V.uniq $ V.map fst v) == V.length v

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
  | otherwise = traverse_ addVar $ V.map fst es

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

withVars :: (MonadError Text m, MonadReader Env m) => Env -> m a -> m a
withVars vs = local (<> vs)

validateExpr :: (MonadError Text m, MonadReader Env m) => Expr -> m ()
validateExpr NumLit{} = pure ()
validateExpr StrLit{} = pure ()
validateExpr (Tuple es) = traverse_ validateExpr es
validateExpr (List es) = traverse_ validateExpr es

validateExpr (Record es)
  | not $ uniqueIdents es = throwError "Duplicate field in record expression"
  | otherwise = traverse_ validateExpr (V.catMaybes $ V.map snd es)

validateExpr (Var v) = asks (S.member v) >>= \case
  False -> throwError $ "Undefined variable " <> showT v
  True -> pure ()

validateExpr (FieldAccess e _) = validateExpr e
validateExpr (Case s bs) = validateExpr s *> traverse_ (uncurry validateBranch) bs
validateExpr (Lambda vs e) = traverse patVars vs >>= \vars -> withVars (fold vars) $ validateExpr e
validateExpr (LambdaCase bs) = traverse_ (uncurry validateBranch) bs
validateExpr (App (Ctor _) es) = traverse_ validateExpr es
validateExpr (App (Fn f) es) = validateExpr f *> traverse_ validateExpr es

validateExpr (Let bs e)
  | not $ uniqueIdents bs = throwError "Duplicate binding in let expression"
  | otherwise =
      withVars (S.fromList $ V.toList $ V.map fst bs) $
        traverse_ validateExpr (V.map snd bs) *> validateExpr e

validate :: MonadError Text m => Env -> BindingGroup -> m BindingGroup
validate prelude bs =
  validateExpr (Let bs $ Var "main") $> bs
  & flip runReaderT prelude
