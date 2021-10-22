module Language.Dynasty.Frontend.Syntax where

import Data.Map.Lazy(Map)
import Data.Text(Text)
import Data.Vector(Vector)

type Ident = Text

-- E for expressions, P for patterns
data NodeKind = E | P

data Node :: NodeKind -> * where
  NumLit :: !Integer -> Node a
  CharLit :: !Char -> Node a
  StrLit :: !Text -> Node a
  RecLit :: !(Map Ident (Node a)) -> Node a
  CtorLit :: !Ident -> !(Vector (Node a)) -> Node a
  Var :: !Ident -> Node a
  RecMember :: !Expr -> !Ident -> Expr
  Case :: !Expr -> !(Vector (Pat, Expr)) -> Expr
  Lam :: !Ident -> !Expr -> Expr
  LamCase :: !(Vector (Pat, Expr)) -> Expr
  App :: !Expr -> !Expr -> Expr
  Let :: !BindingGroup -> !Expr -> Expr

  RecWildcard :: !(Map Ident Pat) -> Pat
  OfType :: !Ident -> !Pat -> Pat
  Wildcard :: Pat
  As :: !Ident -> !Pat -> Pat

deriving stock instance Show (Node a)

type Expr = Node 'E
type Pat = Node 'P

type BindingGroup = Map Ident Expr
