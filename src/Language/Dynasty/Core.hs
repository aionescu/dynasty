{-# LANGUAGE StrictData #-}

module Language.Dynasty.Core where

import Data.Text(Text)

import Language.Dynasty.Syntax(Ident, Num')

data Check
  = IsNum Num'
  | IsStr Text
  | IsCtor Text Int
  | HasField Ident
  deriving stock Show

type Branch = ([(Expr, Check)], Expr)

data Expr
  = NumLit Num'
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
