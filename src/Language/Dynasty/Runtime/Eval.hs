module Language.Dynasty.Runtime.Eval(eval) where

import Control.Monad.Fix(MonadFix(mfix))
import Control.Monad.Reader(runReader, Reader, asks, MonadReader(local))
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

evalExpr (If cond then' else') = do
  c' <- evalExpr cond
  case c' of
    Ctor "True" [] -> evalExpr then'
    Ctor "False" [] -> evalExpr else'
    _ -> pure $ exn "Condition of an If must be True or False"

evalExpr (Let i v e) = do
  v' <- mfix \v' -> local (M.insert i v') $ evalExpr v
  local (M.insert i v') $
    evalExpr e

printVal :: Val -> IO ()
printVal (IO io) = io >>= printVal
printVal (Ctor "Tuple" []) = pure ()
printVal v = print v

runEval :: Reader Env Val -> IO ()
runEval m = printVal $ runReader m prelude

eval :: Expr -> IO ()
eval = runEval . evalExpr
