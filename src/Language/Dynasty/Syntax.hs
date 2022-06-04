{-# LANGUAGE StrictData #-}

module Language.Dynasty.Syntax where

import Data.Scientific(Scientific)
import Data.Text(Text)
import Data.Vector(Vector)

type Ident = Text

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
  FieldAccess :: Expr -> Ident -> Expr
  Case :: Expr -> Vector (Pat, Expr) -> Expr
  Lambda :: Vector Pat -> Expr -> Expr
  LambdaCase :: Vector (Pat, Expr) -> Expr
  App :: AppHead a -> Vector (Syn a) -> Syn a
  Let :: BindingGroup -> Expr -> Expr

  Wildcard :: Pat
  As :: Pat -> Ident -> Pat

deriving stock instance Show (Syn a)

type Expr = Syn 'E
type Pat = Syn 'P

type BindingGroup = Vector (Ident, Expr)
