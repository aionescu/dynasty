{-# LANGUAGE StrictData #-}

module Language.Dynasty.Frontend.Syntax where

import Data.Map.Strict(Map)
import Data.Text(Text)
import Data.Vector(Vector)

type Ident = Text

-- E for expressions, P for patterns
data NodeKind = E | P

data Node a where
  NumLit :: Integer -> Node a
  CharLit :: Char -> Node a
  StrLit :: Text -> Node a
  RecLit :: Map Ident (Node a) -> Node a
  CtorLit :: Ident -> Vector (Node a) -> Node a
  Var :: Ident -> Node a
  RecMember :: Expr -> Ident -> Expr
  Case :: Expr -> Vector (Pat, Expr) -> Expr
  Lam :: Ident -> Expr -> Expr
  LamCase :: Vector (Pat, Expr) -> Expr
  App :: Expr -> Expr -> Expr
  Let :: BindingGroup -> Expr -> Expr

  OfType :: Ident -> Pat -> Pat
  Wildcard :: Pat
  As :: Ident -> Pat -> Pat

deriving stock instance Show (Node a)

type Expr = Node 'E
type Pat = Node 'P

type BindingGroup = Map Ident Expr
