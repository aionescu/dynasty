{-# LANGUAGE StrictData #-}

module Language.Dynasty.Core where

import Data.Text(Text)

import Language.Dynasty.Syntax(Ident, NumLit)

data Check
  = IsNum NumLit
  | IsStr Text
  | IsCtor Text Int
  | HasField Ident
  deriving stock Show

type Branch = ([(Expr, Check)], Expr)

data Expr
  = NumLit NumLit
  | StrLit Text
  | Record [(Ident, Expr)]
  | Ctor Ident [Expr]
  | Var Ident
  | Field Expr Ident
  | Case [Branch]
  | Lambda Ident Expr
  | App Expr Expr
  | Let [(Bool, [(Ident, Expr)])] Expr
  | UnsafeJS Bool [Ident] Text
  deriving stock Show
