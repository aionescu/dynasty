module Language.Dynasty.Frontend.Syntax where

import Data.Map.Strict(Map)

type Ident = String

data Expr
  = IntLit Integer
  | CharLit Char
  | RecLit (Map Ident Expr)
  | Var Ident
  | Ctor Ident [Expr]
  | RecMember Expr Ident
  | Lam Ident Expr
  | App Expr Expr
  | If Expr Expr Expr
  | Let Ident Expr Expr
  deriving stock Show
