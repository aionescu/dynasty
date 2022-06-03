module Language.Dynasty.Codegen(compile) where

import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident, Lit(..))
import Language.Dynasty.Parser(varOpChars)
import Language.Dynasty.Core
import Utils
import Data.Bifunctor(first)

type JS = Text

opCharName :: Char -> Text
opCharName = \case
  '!' -> "Bang"
  '#' -> "Hash"
  '$' -> "Dollar"
  '%' -> "Perc"
  '&' -> "Amp"
  '*' -> "Star"
  '+' -> "Plus"
  '.' -> "Dot"
  '/' -> "Slash"
  '<' -> "Lt"
  '=' -> "Eq"
  '>' -> "Gt"
  '?' -> "Qmark"
  '@' -> "At"
  '\\' -> "Bslash"
  '^' -> "Caret"
  '|' -> "Bar"
  '-' -> "Minus"
  '~' -> "Tilde"
  ';' -> "Semi"
  c -> error $ "opCharName: " <> show c

ident :: Ident -> JS
ident i
  | T.all (`elem` varOpChars) i = "$" <> T.concatMap opCharName i
  | otherwise = T.replace "'" "_" i

isWhnf :: Expr -> Bool
isWhnf Lit{} = True
isWhnf Ctor{} = True
isWhnf Record{} = True
isWhnf Lambda{} = True
isWhnf _ = False

thunk :: Expr -> JS
thunk (Var i) = ident i
thunk e
  | isWhnf e = "{$v:" <> compileExpr e <> "}"
  | otherwise = "{$f:()=>" <> compileExpr e <> "}"

compileLet :: Expr -> JS
compileLet e' = "(()=>{" <> bindings <> "return " <> compileExpr e <> ";})()"
  where
    bindings = foldMap (\(i, v) -> "const " <> ident i <> "=" <> thunk v <> ";") bs
    (bs, e) = packLets e'

    packLets (Let i v s) = first ((i, v) :) $ packLets s
    packLets s = ([], s)

compileLetRec :: Vector (Ident, Expr) -> Expr -> JS
compileLetRec bs e = "(()=>{" <> decls <> assigns <> "return " <> compileExpr e <> ";})()"
  where
    decls = foldMap ((\i -> "const " <> ident i <> "={};") . fst) bs
    assigns = foldMap (\(i, v) -> ident i <> ".$f=()=>" <> compileExpr v <> ";") bs

check :: JS -> Check -> JS
check e (IsLit (Int i)) = e <> "===" <> showT i
check e (IsLit (Char c)) = e <> "===" <> showT c
check e (IsLit (Str s)) = e <> "===" <> showT s
check e (IsCtor c) = e <> ".$" <> "===" <> showT c
check e IsRecord = "typeof(" <> e <> ")==='object'&&" <> e <> ".$===undefined"
check e (HasFields n) = "(Object.keys(" <> e <> ").length-1)===" <> showT n
check e (HasRecField f) = e <> "." <> ident f <> "!==undefined"
check _ NoOp = ""

splitDig :: JS -> Dig -> ([JS], [(JS, Ident)])
splitDig e (Field f d) = splitDig ("$e(" <> e <> ".$" <> showT f <> ")") d
splitDig e (RecField f d) = splitDig ("$e(" <> e <> "." <> ident f <> ")") d
splitDig e (TypeOf d) = splitDig ("typeOf(" <> e <> ")") d

splitDig e (And a b) = (c <> c', i <> i')
  where
    (c, i) = splitDig e a
    (c', i') = splitDig e b

splitDig e (Check c) = ([check e c], [])
splitDig e (Assign i) = ([], [(e, i)])

compileCase :: Ident -> Vector (Dig, Expr) -> JS
compileCase s bs = "(" <> foldr branch "()=>{throw 'Incomplete pattern match';}" bs <> ")"
  where
    branch (d, e) r = cond <> "?" <> withDecls <> ":" <> r
      where
        (cs, as) = splitDig ("$e(" <> s <> ")") d
        boolExpr = T.intercalate "&&" $ filter (not . T.null) cs

        cond
          | T.null boolExpr = "true"
          | otherwise = boolExpr

        withDecls
          | [] <- as = compileExpr e
          | otherwise = "(()=>{" <> decls <> "return " <> compileExpr e <> ";})()"
          where
            decls = foldMap (\(path, i) -> "const " <> ident i <> "={$v:" <> path <> "};") as

compileExpr :: Expr -> JS
compileExpr (Lit (Int i)) = showT i
compileExpr (Lit (Char c)) = showT c
compileExpr (Lit (Str s)) = showT s

compileExpr (Record fs) = "{" <> fields <> "}"
  where
    fields = T.intercalate "," $ V.toList $ V.map (\(i, e) -> ident i <> ":" <> thunk e) fs

compileExpr (Ctor c es) = "{$:" <> showT c <> fields <> "}"
  where
    fields = V.foldMap id $ V.imap (\i e -> ",$" <> showT i <> ":" <> thunk e) es

compileExpr (Var i) = "$e(" <> ident i <> ")"
compileExpr (FieldAccess e i) = compileExpr e <> ident i
compileExpr (Case i bs) = compileCase i bs

compileExpr (Lambda i e) = "(" <> ident i <> "=>" <> parenthesize e <> ")"
  where
    parenthesize (compileExpr -> js)
      | T.isPrefixOf "{" js = "(" <> js <> ")"
      | otherwise = js

compileExpr (App f a) = compileExpr f <> "(" <> thunk a <> ")"
compileExpr e@Let{} = compileLet e
compileExpr (LetRec bs e) = compileLetRec bs e

compile :: JS -> Expr -> JS
compile prelude e = prelude <> compileExpr e <> ".$r();"
