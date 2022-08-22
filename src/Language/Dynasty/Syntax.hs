{-# LANGUAGE StrictData #-}

module Language.Dynasty.Syntax where

import Data.Scientific(Scientific)
import Data.Text(Text)

type Ident = Text
type Idx = Int

data NumLit
  = NaN
  | Inf
  | NegInf
  | Num Scientific
  deriving stock Show

-- E for expressions, P for patterns
data SynKind = E | P

data AppHead (k :: SynKind) where
  Ctor :: Ident -> AppHead k
  Fn :: Syn E -> AppHead E

deriving stock instance Show (AppHead k)

data Syn (k :: SynKind) where
  NumLit :: NumLit -> Syn k
  StrLit :: Text -> Syn k
  Tuple :: [Syn k] -> Syn k
  List :: [Syn k] -> Syn k
  Record :: [(Ident, Maybe (Syn k))] -> Syn k
  Var :: Ident -> Syn k

  RecordField :: Syn E -> Ident -> Syn E
  CtorField :: Syn E -> Idx -> Syn E
  Case :: Syn E -> [(Syn P, Syn E)] -> Syn E
  Lambda :: [Syn P] -> Syn E -> Syn E
  LambdaCase :: [(Syn P, Syn E)] -> Syn E
  App :: AppHead k -> [Syn k] -> Syn k
  Let :: BindingGroup -> Syn E -> Syn E
  Do :: [(Maybe Ident, Syn E)] -> Syn E -> Syn E
  UnsafeJS :: Bool -> [Ident] -> Text -> Syn E

  Wildcard :: Syn P
  As :: Syn P -> Ident -> Syn P

deriving stock instance Show (Syn k)

type Expr = Syn E
type Pat = Syn P

type BindingGroup = [(Ident, Expr)]

data Import =
  Import
  { importModule :: Text
  , importIdents :: Maybe [Ident]
  }
  deriving stock Show

data Module =
  Module
  { moduleName :: Text
  , moduleExports :: [Ident]
  , moduleImports :: [Import]
  , moduleBindings :: BindingGroup
  }
  deriving stock Show
