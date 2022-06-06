module Language.Dynasty.Codegen(compile) where

import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident, Num'(..))
import Language.Dynasty.Parser(varOpChars)
import Language.Dynasty.Core
import Utils

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
  | T.head i == '_' || T.head i == '$' = i
  | otherwise = "_" <> T.replace "'" "_" i

compileLet :: Vector (Bool, Vector (Ident, Expr)) -> Expr -> JS
compileLet gs e = "(()=>{" <> foldMap bindingGroup gs <> "return " <> compileExpr e <> ";})()"
  where
    bindingGroup (False, bs) = foldMap (\(i, v) -> "const " <> ident i <> "=" <> thunk v <> ";") bs
    bindingGroup (True, bs) = decls <> assigns
      where
        decls = foldMap ((\i -> "const " <> ident i <> "={};") . fst) bs
        assigns = foldMap (\(i, v) -> ident i <> ".f=()=>" <> parenthesize v <> ";") bs

check :: JS -> Check -> JS
check e (IsNum NaN) = "isNaN(" <> e <> ")"
check e (IsNum Inf) = e <> "===Infinity"
check e (IsNum NegInf) = e <> "===-Infinity"
check e (IsNum (Num n)) = e <> "===" <> showT n
check e (IsStr s) = e <> "===" <> showT s
check e (IsCtor c 0) = e <> ".$===" <> showT c <> "&&!" <> e <> ".$0"
check e (IsCtor c n) =
  e <> ".$===" <> showT c <> "&&" <> e <> ".$" <> showT (pred n) <> "&&!" <> e <> ".$" <> showT n
check e (HasField f) = e <> "." <> ident f

compileCase :: Vector Branch -> JS
compileCase = foldr branch "(()=>{throw 'Incomplete pattern match';})()"
  where
    branch (cs, e) r
      | V.null cs = compileExpr e
      | otherwise = cond <> "?" <> compileExpr e <> ":" <> r
      where
        cond = T.intercalate "&&" $ V.toList $ (\(e', c) -> check (compileExpr e') c) <$> cs

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
compileExpr (Field e i) = "e(" <> compileExpr e <> "." <> ident i <> ")"
compileExpr (Case bs) = compileCase bs
compileExpr (Lambda i e) = "(" <> ident i <> "=>" <> parenthesize e <> ")"
compileExpr (App f a) = compileExpr f <> "(" <> thunk a <> ")"
compileExpr (Let bs e) = compileLet bs e

isWhnf :: Expr -> Bool
isWhnf NumLit{} = True
isWhnf StrLit{} = True
isWhnf Ctor{} = True
isWhnf Record{} = True
isWhnf Lambda{} = True
isWhnf _ = False

thunk :: Expr -> JS
thunk (Var i) = ident i
thunk (Field e i) = compileExpr e <> "." <> ident i
thunk e
  | isWhnf e = "{v:" <> compileExpr e <> "}"
  | otherwise = "{f:()=>" <> parenthesize e <> "}"

parenthesize :: Expr -> JS
parenthesize (compileExpr -> js)
  | T.isPrefixOf "{" js = "(" <> js <> ")"
  | otherwise = js

compile :: JS -> Expr -> JS
compile prelude e = prelude <> "\n" <> compileExpr e <> ".r();\n"
