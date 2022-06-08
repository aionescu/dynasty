{-# LANGUAGE StrictData #-}

module Language.Dynasty.Syntax where

import Data.Scientific(Scientific)
import Data.Text(Text)

type Ident = Text
type Idx = Int

-- E for expressions, P for patterns
data SynKind = E | P

data Num'
  = NaN
  | Inf
  | NegInf
  | Num Scientific
  deriving stock Show

data AppHead :: SynKind -> * where
  Ctor :: Ident -> AppHead a
  Fn :: Syn 'E -> AppHead 'E

deriving stock instance Show (AppHead a)

data Syn :: SynKind -> * where
  NumLit :: Num' -> Syn a
  StrLit :: Text -> Syn a
  Tuple :: [Syn a] -> Syn a
  List :: [Syn a] -> Syn a
  Record :: [(Ident, Maybe (Syn a))] -> Syn a
  Var :: Ident -> Syn a
  RecordField :: Syn 'E -> Ident -> Syn 'E
  CtorField :: Syn 'E -> Idx -> Syn 'E
  Case :: Syn 'E -> [(Syn 'P, Syn 'E)] -> Syn 'E
  Lambda :: [Syn 'P] -> Syn 'E -> Syn 'E
  LambdaCase :: [(Syn 'P, Syn 'E)] -> Syn 'E
  App :: AppHead a -> [Syn a] -> Syn a
  Let :: BindingGroup -> Syn 'E -> Syn 'E
  UnsafeJS :: Bool -> [Ident] -> Text -> Syn 'E

  Wildcard :: Syn 'P
  As :: Syn 'P -> Ident -> Syn 'P

deriving stock instance Show (Syn a)

type Expr = Syn 'E
type Pat = Syn 'P

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
