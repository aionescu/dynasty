module Language.Dynasty.Frontend.Syntax where

import Data.Map.Lazy(Map)

type Ident = String

-- E for expressions, P for patterns
data ExprKind = E | P

data Expr :: ExprKind -> * where
  NumLit :: Integer -> Expr a
  CharLit :: Char -> Expr a
  RecLit :: Map Ident (Expr a) -> Expr a
  CtorLit :: Ident -> [Expr a] -> Expr a
  Var :: Ident -> Expr a
  RecMember :: Expr 'E -> Ident -> Expr 'E
  Lam :: Ident -> Expr 'E -> Expr 'E
  App :: Expr 'E -> Expr 'E -> Expr 'E
  Let :: Ident -> Expr 'E -> Expr 'E -> Expr 'E
  Match :: Expr 'E -> [(Expr 'P, Expr 'E)] -> Expr 'E
  OfType :: Ident -> Expr 'P -> Expr 'P
  Wildcard :: Expr 'P

deriving instance Show (Expr a)
