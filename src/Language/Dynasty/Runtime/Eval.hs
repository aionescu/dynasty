module Language.Dynasty.Runtime.Eval(eval) where

import Control.Monad(zipWithM_)
import Control.Monad.Except(MonadError(throwError))
import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.Reader(runReader, Reader, asks, MonadReader(ask, local))
import Control.Monad.State(execStateT, modify, MonadState)
import Data.Foldable(traverse_)
import Data.Functor((<&>))
import qualified Data.Map.Lazy as M
import Data.Maybe(fromMaybe)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val
import Language.Dynasty.Runtime.Prelude

evalExpr :: (MonadFix m, MonadReader Env m) => Expr -> m Val
evalExpr (NumLit i) = pure $ Num i
evalExpr (CharLit b) = pure $ Char b
evalExpr (Var i) = asks $ fromMaybe (exn "Variable not declared") . M.lookup i

evalExpr (RecLit m) = Rec <$> traverse evalExpr m

evalExpr (CtorLit i es) = Ctor i <$> traverse evalExpr es

evalExpr (Lam i e) =
  asks \env ->
    Fn \a ->
      runReader (evalExpr e) (M.insert i a env)

evalExpr (RecMember e i) = evalExpr e <&> \case
  Rec m -> fromMaybe (exn "Field not found.") $ M.lookup i m
  _ -> exn "Need record for getter"

evalExpr (App f a) =
  evalExpr f >>= \case
    Fn f' -> f' <$> evalExpr a
    _ -> pure $ exn "LHS of App must be a function."

evalExpr (Let bs e) = do
  vs' <- mfix \vs' ->
    local (M.union vs') $
      traverse evalExpr bs

  local (M.union vs') $
    evalExpr e

evalExpr (Match e ps) = evalExpr e >>= \v -> tryBranches v ps

tryBranch :: Env -> Val -> Pat -> Maybe Env
tryBranch env v p =
    case execStateT (evalPat v p) env of
    Left _ -> Nothing
    Right e -> Just e

tryBranches :: (MonadFix m, MonadReader Env m) => Val -> [(Pat, Expr)] -> m Val
tryBranches _ [] = pure $ exn "Incomplete pattern match"
tryBranches v ((p, e) : ps) =
  ask >>= \env ->
    case tryBranch env v p of
      Nothing -> tryBranches v ps
      Just newEnv -> local (const newEnv) $ evalExpr e

fail' :: MonadError () m => m a
fail' = throwError ()

evalPat :: (MonadState Env m, MonadError () m) => Val -> Pat -> m ()
evalPat v (Var i) = modify (M.insert i v)
evalPat (Num n) (NumLit m)
  | m == n = pure ()
  | otherwise = fail'
evalPat (Char c) (CharLit d)
  | c == d = pure ()
  | otherwise = fail'
evalPat (Ctor c vs) (CtorLit c' ps)
  | c == c' && length vs == length ps = zipWithM_ evalPat vs ps
evalPat (Rec m) (RecLit m')
  | M.null $ M.difference m' m = traverse_ (uncurry evalPat) $ M.elems $ M.intersectionWith (,) m m'
  | otherwise = fail'
evalPat (Rec m) (RecWildcard m')
  | M.null $ M.difference m' m = do
      traverse_ (uncurry evalPat) $ M.elems $ M.intersectionWith (,) m m'
      modify $ M.union $ M.difference m m'
  | otherwise = fail'
evalPat v (OfType i p) = evalPat (typeOf v) p *> modify (M.insert i v)
evalPat _ Wildcard = pure ()
evalPat _ _ = fail'

printVal :: Val -> IO ()
printVal (IO io) = io >>= printVal
printVal (Ctor "Tuple" []) = pure ()
printVal v = print v

runEval :: Reader Env Val -> IO ()
runEval m = printVal $ runReader m prelude

eval :: Expr -> IO ()
eval = runEval . evalExpr
