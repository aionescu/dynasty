{-# LANGUAGE StrictData #-}

module Language.Dynasty.Frontend.Syntax where

import Data.Text(Text)
import Data.Vector(Vector)

type Ident = Text

-- E for expressions, P for patterns
data NodeKind = E | P

data Node a where
  NumLit :: Integer -> Node a
  CharLit :: Char -> Node a
  StrLit :: Text -> Node a
  TupLit :: Vector (Node a) -> Node a
  ListLit :: Vector (Node a) -> Node a
  RecLit :: Vector (Ident, Maybe (Node a)) -> Node a
  CtorLit :: Ident -> Vector (Node a) -> Node a
  Var :: Ident -> Node a
  RecMember :: Expr -> Ident -> Expr
  Case :: Expr -> Vector (Pat, Expr) -> Expr
  Lam :: Vector Pat -> Expr -> Expr
  LamCase :: Vector (Pat, Expr) -> Expr
  App :: Expr -> Expr -> Expr
  Let :: BindingGroup -> Expr -> Expr

  Wildcard :: Pat
  OfType :: Ident -> Pat -> Pat
  As :: Ident -> Pat -> Pat

deriving stock instance Show (Node a)

type Expr = Node 'E
type Pat = Node 'P

type BindingGroup = Vector (Ident, Expr)
