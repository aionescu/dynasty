{-# LANGUAGE StrictData #-}

module Language.Dynasty.Core where

import Data.Text(Text)

import Language.Dynasty.Syntax(Id, Name, Number)

data Check
  = IsNum Number
  | IsStr Text
  | IsCtor Text Int
  | HasField Id
  deriving stock Show

type Branch = ([(Expr, Check)], Expr)

data Expr
  = NumLit Number
  | StrLit Text
  | RecLit [(Id, Expr)]
  | CtorLit Id [Expr]
  | Var Name
  | Field Expr Id
  | Case [Branch]
  | Lam Id Expr
  | App Expr Expr
  | Let BindingGroup Expr
  | UnsafeJS Bool [Id] Text
  deriving stock Show

type BindingGroup = [(Bool, [(Id, Expr)])]

data Module
  = Module
  { moduleName :: Id
  , moduleInitCode :: Text
  , moduleBindings :: BindingGroup
  , moduleExports :: [Id]
  }
  deriving stock Show
