module Language.Dynasty.Runtime.Eval(eval) where

import Control.Monad.Reader(runReader, Reader, asks, MonadReader(local))
import Data.Functor((<&>))
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val
import Language.Dynasty.Runtime.Prelude

evalExpr :: MonadReader Env m => Expr -> m Val
evalExpr (IntLit i) = pure $ Int i
evalExpr (CharLit b) = pure $ Char b
evalExpr (Var i) = asks $ fromMaybe (exn "Variable not declared") . M.lookup i

evalExpr (RecLit m) = Rec <$> traverse evalExpr m

evalExpr (Ctor i es) = VCtor i <$> traverse evalExpr es

evalExpr (Lam i e) =
  asks \env ->
    VFun \a ->
      runReader (evalExpr e) (M.insert i a env)

evalExpr (RecMember e i) = evalExpr e <&> \case
  Rec m -> fromMaybe (exn "Field not found.") $ M.lookup i m
  _ -> exn "Need record for getter"

evalExpr (App f a) =
  evalExpr f >>= \case
    VFun f' -> f' <$> evalExpr a
    _ -> pure $ exn "LHS of App must be a function."

evalExpr (If cond then' else') = do
  c' <- evalExpr cond
  case c' of
    VCtor "True" [] -> evalExpr then'
    VCtor "False" [] -> evalExpr else'
    _ -> pure $ exn "Condition of an If must be True or False"

evalExpr (Let i v e) = do
  v' <- evalExpr v
  local (M.insert i v') $
    evalExpr e

printVal :: Val -> IO ()
printVal (IO io) = io >>= printVal
printVal (VCtor "Tuple" []) = pure ()
printVal v = print v

runEval :: Reader Env Val -> IO ()
runEval m = printVal $ runReader m prelude

eval :: Expr -> IO ()
eval = runEval . evalExpr
