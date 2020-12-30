module Language.Dynasty.Frontend.Syntax where

import Data.Map.Lazy(Map)

type Ident = String

-- E for expressions, P for patterns
data NodeKind = E | P

data Node :: NodeKind -> * where
  NumLit :: Integer -> Node a
  CharLit :: Char -> Node a
  RecLit :: Map Ident (Node a) -> Node a
  CtorLit :: Ident -> [Node a] -> Node a
  Var :: Ident -> Node a
  RecMember :: Expr -> Ident -> Expr
  Lam :: Ident -> Expr -> Expr
  App :: Expr -> Expr -> Expr
  Let :: BindingGroup -> Expr -> Expr
  Match :: Expr -> [(Pat, Expr)] -> Expr
  OfType :: Ident -> Pat -> Pat
  Wildcard :: Pat

deriving instance Show (Node a)

type Expr = Node 'E
type Pat = Node 'P

type BindingGroup = Map Ident Expr
