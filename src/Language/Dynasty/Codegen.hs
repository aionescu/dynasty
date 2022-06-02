module Language.Dynasty.Codegen(compile) where

import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Language.Dynasty.Syntax(Ident, Lit(..))
import Language.Dynasty.Core
import Utils
import Data.Char (isAlphaNum)
import Data.Bifunctor (first)

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
  | T.any (\c -> isAlphaNum c || c == '\'' || c == '$') i = T.replace "'" "_" i
  | otherwise = "$" <> T.concatMap opCharName i

isWhnf :: Expr -> Bool
isWhnf Lit{} = True
isWhnf Ctor{} = True
isWhnf Record{} = True
isWhnf Lambda{} = True
isWhnf _ = False

thunk :: Expr -> JS
thunk (Var i) = ident i
thunk e
  | isWhnf e = "_v(" <> compileExpr e <> ")"
  | otherwise = "_f(()=>" <> compileExpr e <> ")"

compileLet :: Expr -> JS
compileLet e' = "(() => {" <> bindings <> "return " <> compileExpr e <> ";})()"
  where
    bindings = foldMap (\(i, v) -> "const " <> ident i <> "=" <> thunk v <> ";") bs
    (bs, e) = packLets e'

    packLets (Let i v s) = first ((i, v) :) $ packLets s
    packLets s = ([], s)

compileExpr :: Expr -> JS
compileExpr (Lit (Int i)) = showT i
compileExpr (Lit (Char c)) = showT c
compileExpr (Lit (Str s)) = showT s

compileExpr (Record fs) = "{" <> fields <> "}"
  where
    fields = T.intercalate "," $ V.toList $ V.map (\(i, e) -> ident i <> ":" <> thunk e) fs

compileExpr (Ctor c es) = "{_c:" <> showT c <> fields <> "}"
  where
    fields = V.foldMap id $ V.imap (\i e -> ",_" <> showT i <> ":" <> thunk e) es

compileExpr (Var i) = "_e(" <> ident i <> ")"
compileExpr (FieldAccess e i) = compileExpr e <> ident i
compileExpr (Case _i _bs) = error "Case not supported"
compileExpr (Lambda i e) = "(" <> ident i <> "=>" <> compileExpr e <> ")"
compileExpr (App f a) = compileExpr f <> "(" <> thunk a <> ")"
compileExpr e@Let{} = compileLet e
compileExpr (LetRec _bs _e) = error "LetRec not supported"

compile :: JS -> Expr -> JS
compile prelude e = prelude <> compileExpr e <> "._r();"
