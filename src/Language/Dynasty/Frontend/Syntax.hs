module Language.Dynasty.Frontend.Syntax where

import Data.Map.Strict(Map)

type Ident = String

data Expr
  = NumLit Integer
  | CharLit Char
  | RecLit (Map Ident Expr)
  | Var Ident
  | CtorLit Ident [Expr]
  | RecMember Expr Ident
  | Lam Ident Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Let Ident Expr Expr
  deriving stock Show
