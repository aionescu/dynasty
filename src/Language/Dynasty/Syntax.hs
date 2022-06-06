{-# LANGUAGE StrictData #-}

module Language.Dynasty.Syntax where

import Data.Scientific(Scientific)
import Data.Text(Text)
import Data.Vector(Vector)

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
  Tuple :: Vector (Syn a) -> Syn a
  List :: Vector (Syn a) -> Syn a
  Record :: Vector (Ident, Maybe (Syn a)) -> Syn a
  Var :: Ident -> Syn a
  RecordField :: Syn 'E -> Ident -> Syn 'E
  CtorField :: Syn 'E -> Idx -> Syn 'E
  Case :: Syn 'E -> Vector (Syn 'P, Syn 'E) -> Syn 'E
  Lambda :: Vector (Syn 'P) -> Syn 'E -> Syn 'E
  LambdaCase :: Vector (Syn 'P, Syn 'E) -> Syn 'E
  App :: AppHead a -> Vector (Syn a) -> Syn a
  Let :: BindingGroup -> Syn 'E -> Syn 'E

  Wildcard :: Syn 'P
  As :: Syn 'P -> Ident -> Syn 'P

deriving stock instance Show (Syn a)

type Expr = Syn 'E
type Pat = Syn 'P

type BindingGroup = Vector (Ident, Expr)
