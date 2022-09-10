{-# LANGUAGE StrictData #-}

module Language.Dynasty.Syntax where

import Data.Scientific(Scientific)
import Data.Text(Text)

type Id = Text
type Idx = Int

data Number
  = NaN
  | Inf
  | NegInf
  | Num Scientific
  deriving stock Show

data Name
  = Qual Id Id
  | Unqual Id
  deriving stock Show

data Pat
  = NumPat Number
  | StrPat Text
  | TupPat [Pat]
  | ListPat [Pat]
  | CtorPat Id [Pat]
  | RecPat [(Id, Maybe Pat)]
  | VarPat Id
  | Wildcard
  | As Id Pat
  deriving stock Show

data Expr
  = NumLit Number
  | StrLit Text
  | TupLit [Expr]
  | ListLit [Expr]
  | RecLit [(Id, Maybe Expr)]
  | CtorLit Id [Expr]
  | Var Name
  | RecField Expr Id
  | CtorField Expr Idx
  | Case Expr [(Pat, Expr)]
  | Lam [Pat] Expr
  | LamCase [(Pat, Expr)]
  | App Expr Expr
  | Let BindingGroup Expr
  | Do Name [(Maybe Pat, Expr)] Expr
  | UnsafeJS Bool [Id] Text
  deriving stock Show

type BindingGroup = [(Id, Expr)]

data Module =
  Module
  { moduleName :: Id
  , moduleInitCode :: Text
  , moduleImports :: [Id]
  , moduleBindings :: BindingGroup
  }
  deriving stock Show

data Program =
  Program
  { programMainModule :: Id
  , programReachable :: [Id]
  , programModules :: [Module]
  }
  deriving stock Show
