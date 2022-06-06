{-# LANGUAGE StrictData #-}

module Language.Dynasty.Core where

import Data.Text(Text)
import Data.Vector(Vector)

import Language.Dynasty.Syntax(Ident, Num')

data Check
  = IsNum Num'
  | IsStr Text
  | IsCtor Text Int
  | HasField Ident
  deriving stock Show

type Branch = (Vector (Expr, Check), Expr)

data Expr
  = NumLit Num'
  | StrLit Text
  | Record (Vector (Ident, Expr))
  | Ctor Ident (Vector Expr)
  | Var Ident
  | Field Expr Ident
  | Case (Vector Branch)
  | Lambda Ident Expr
  | App Expr Expr
  | Let Ident Expr Expr
  | LetRec (Vector (Ident, Expr)) Expr
  deriving stock Show
