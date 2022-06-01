{-# LANGUAGE StrictData #-}

module Language.Dynasty.Frontend.Core where

import Data.Text(Text)
import Data.Vector(Vector)

type Ident = Text

-- In Core there are no patterns.
-- Instead, each branch of a case expression becomes a pair of "checks" and "assignments".

-- Dig into a value.
data Dig
  = DigField Int
  | DigRecField Ident
  | DigTypeOf
  deriving stock Show

type Digs = Vector Dig

-- Check properties of a value.
data Check
  = IsIntLit Integer
  | IsCharLit Char
  | IsStrLit Text
  | IsCtor Text
  | IsRecord
  | HasFields Int
  | HasRecField Ident
  deriving stock Show

-- Pair of checks and assignments.
data CaseBranch = CaseBranch (Vector (Digs, Check)) (Vector (Digs, Ident))
  deriving stock Show

data Expr
  = NumLit Integer
  | CharLit Char
  | StrLit Text
  | RecLit (Vector (Ident, Expr))
  | CtorLit Ident (Vector Expr)
  | Var Ident
  | RecMember Expr Ident
  | Case Expr (Vector CaseBranch)
  | Lam Ident Expr
  | App Expr Expr
  | Let Ident Expr Expr
  | LetRec (Vector (Ident, Expr)) Expr
  deriving stock Show
