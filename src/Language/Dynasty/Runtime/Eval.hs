module Language.Dynasty.Runtime.Eval(runTopLevel) where

import Control.Monad(zipWithM_)
import Control.Monad.Except(MonadError(throwError))
import Control.Monad.Fix(MonadFix)
import Control.Monad.Reader(runReader, Reader, asks, MonadReader(ask, local))
import Control.Monad.State(execStateT, modify, MonadState)
import Data.Foldable(traverse_)
import Data.Functor((<&>))
import qualified Data.Map.Lazy as M
import Data.Maybe(fromMaybe)
import Data.Text(Text)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val
import Language.Dynasty.Runtime.Prelude
import Utils

evalExpr :: (MonadFix m, MonadReader Env m) => Expr -> m Val
evalExpr (NumLit i) = pure $ Num i
evalExpr (CharLit b) = pure $ Char b
evalExpr (StrLit s) = pure $ Str s
evalExpr (Var i) = asks $ fromMaybe (exn $ "Variable '" <> i <> "' does not exist in the current scope.") . M.lookup i

evalExpr (RecLit m) = Rec <$> traverse evalExpr m

evalExpr (CtorLit i es) = Ctor i <$> traverse evalExpr es

evalExpr (Lam ps) =
  asks \env ->
    Fn \v ->
      runReader (tryBranches v ps) env

evalExpr (RecMember e i) = evalExpr e <&> \case
  v@(Rec m) -> fromMaybe (exn $ "Field '" <> i <> "' does exist in the record: " <> showT v <> ".") $ M.lookup i m
  v -> exn $ "Tried to read field '" <> i <> "' of non-record: " <> showT v <> "."

evalExpr (App f a) =
  evalExpr f >>= \case
    Fn f' -> f' <$> evalExpr a
    e@(Ctor "Exception" _) -> pure e
    v -> pure $ exn $ "Tried to apply non-function: " <> showT v <> "."

evalExpr (Let bs e) = mdo
  vs' <-
    local (M.union vs') $
      traverse evalExpr bs

  local (M.union vs') $
    evalExpr e

tryBranch :: Env -> Val -> Pat -> Maybe Env
tryBranch env v p =
    case execStateT (evalPat v p) env of
    Left _ -> Nothing
    Right e -> Just e

tryBranches :: (MonadFix m, MonadReader Env m) => Val -> [(Pat, Expr)] -> m Val
tryBranches v [] = pure $ exn $ "Incomplete pattern match on: " <> showT v <> "."
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
evalPat v (As i p) = evalPat v p *> modify (M.insert i v)
evalPat _ _ = fail'

printVal :: Val -> IO ()
printVal (IO io) = io >>= printVal
printVal (Ctor "Tuple" []) = pure ()
printVal v = print v

runEval :: [Text] -> Reader Env Val -> IO ()
runEval args m = printVal $ runReader m $ M.insert "prelude" (Rec p) p
  where
    p = prelude args

runTopLevel :: [Text] -> BindingGroup  -> IO ()
runTopLevel args bs = runEval args $ evalExpr $ Let bs $ Var "main"
