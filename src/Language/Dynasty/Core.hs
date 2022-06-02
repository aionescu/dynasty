{-# LANGUAGE StrictData #-}

module Language.Dynasty.Core where

import Data.Text(Text)
import Data.Vector(Vector)

import Language.Dynasty.Syntax(Ident, Lit)

-- In Core there are no patterns.
-- Instead, each branch of a case expression becomes a pair of "checks" and "assignments".

-- Dig into a value.
data Dig
  = Field Int Dig
  | RecField Ident Dig
  | TypeOf Dig
  | And Dig Dig
  | Check Check
  | Assign Ident
  deriving stock Show

-- Check properties of a value.
data Check
  = IsLit Lit
  | IsCtor Text
  | IsRecord
  | HasFields Int
  | HasRecField Ident
  | NoOp
  deriving stock Show

data Expr
  = Lit Lit
  | Record (Vector (Ident, Expr))
  | Ctor Ident (Vector Expr)
  | Var Ident
  | FieldAccess Expr Ident
  | Case Ident (Vector (Dig, Expr))
  | Lambda Ident Expr
  | App Expr Expr
  | Let Ident Expr Expr
  | LetRec (Vector (Ident, Expr)) Expr
  deriving stock Show
