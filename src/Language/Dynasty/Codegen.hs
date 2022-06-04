module Language.Dynasty.Codegen(compile) where

import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident, Num'(..))
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
  | T.all (`elem` varOpChars) i = "_" <> T.concatMap opCharName i
  | T.head i == '_' = i
  | otherwise = "_" <> T.replace "'" "_" i

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
    assigns = foldMap (\(i, v) -> ident i <> ".f=()=>" <> parenthesize v <> ";") bs

check :: JS -> Check -> JS
check e (IsNum NaN) = "isNaN(" <> e <> ")"
check e (IsNum Inf) = e <> "===Infinity"
check e (IsNum NegInf) = e <> "===-Infinity"
check e (IsNum (Num n)) = e <> "===" <> showT n
check e (IsStr s) = e <> "===" <> showT s
check e (IsCtor c) = e <> ".$" <> "===" <> showT c
check e IsRecord = "typeof(" <> e <> ")==='object'&&" <> e <> ".$===undefined"
check e (HasFields n) = "(Object.keys(" <> e <> ").length-1)===" <> showT n
check e (HasRecField f) = e <> "." <> ident f <> "!==undefined"
check _ NoOp = ""

splitDig :: JS -> Dig -> ([JS], [(JS, Ident)])
splitDig e (Field f d) = splitDig ("e(" <> e <> ".$" <> showT f <> ")") d
splitDig e (RecField f d) = splitDig ("e(" <> e <> "." <> ident f <> ")") d

splitDig e (And a b) = (c <> c', i <> i')
  where
    (c, i) = splitDig e a
    (c', i') = splitDig e b

splitDig e (Check c) = ([check e c], [])
splitDig e (Assign i) = ([], [(e, i)])

compileCase :: Ident -> Expr -> Vector (Dig, Expr) -> JS
compileCase v s bs =
  "(()=>{const " <> ident v <> "=" <> compileExpr s <> ";return "
  <> foldr branch "()=>{throw 'Incomplete pattern match';}" bs <> ";})()"
  where
    branch (d, e) r = cond <> "?" <> withDecls <> ":" <> r
      where
        (cs, as) = splitDig v d
        boolExpr = T.intercalate "&&" $ filter (not . T.null) cs

        cond
          | T.null boolExpr = "true"
          | otherwise = boolExpr

        withDecls
          | [] <- as = compileExpr e
          | otherwise = "(()=>{" <> decls <> "return " <> compileExpr e <> ";})()"
          where
            decls = foldMap (\(path, i) -> "const " <> ident i <> "={v:" <> path <> "};") as

compileExpr :: Expr -> JS
compileExpr (NumLit NaN) = "NaN"
compileExpr (NumLit Inf) = "Infinity"
compileExpr (NumLit NegInf) = "-Infinity"
compileExpr (NumLit (Num n)) = showT n
compileExpr (StrLit s) = showT s

compileExpr (Record fs) = "{" <> fields <> "}"
  where
    fields = T.intercalate "," $ V.toList $ V.map (\(i, e) -> ident i <> ":" <> thunk e) fs

compileExpr (Ctor c es) = "{$:" <> showT c <> fields <> "}"
  where
    fields = V.foldMap id $ V.imap (\i e -> ",$" <> showT i <> ":" <> thunk e) es

compileExpr (Var i) = "e(" <> ident i <> ")"
compileExpr (FieldAccess e i) = compileExpr e <> ident i
compileExpr (Case i e bs) = compileCase i e bs

compileExpr (Lambda i e) = "(" <> ident i <> "=>" <> parenthesize e <> ")"

compileExpr (App f a) = compileExpr f <> "(" <> thunk a <> ")"
compileExpr e@Let{} = compileLet e
compileExpr (LetRec bs e) = compileLetRec bs e

isWhnf :: Expr -> Bool
isWhnf NumLit{} = True
isWhnf StrLit{} = True
isWhnf Ctor{} = True
isWhnf Record{} = True
isWhnf Lambda{} = True
isWhnf _ = False

thunk :: Expr -> JS
thunk (Var i) = ident i
thunk e
  | isWhnf e = "{v:" <> compileExpr e <> "}"
  | otherwise = "{f:()=>" <> parenthesize e <> "}"

parenthesize :: Expr -> JS
parenthesize (compileExpr -> js)
  | T.isPrefixOf "{" js = "(" <> js <> ")"
  | otherwise = js

compile :: JS -> Expr -> JS
compile prelude e = prelude <> "\n" <> compileExpr e <> ".r();\n"
