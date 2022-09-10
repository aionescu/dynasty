module Language.Dynasty.Codegen(compile) where

import Data.Foldable(foldMap')
import Data.Text(Text)
import Data.Text qualified as T

import Language.Dynasty.Core
import Language.Dynasty.Parser(varOpChars)
import Language.Dynasty.Syntax(Id, Name(..), Number(..))
import Language.Dynasty.Utils(imap, showT)

type JS = Text

opCharName :: Char -> JS
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
  '-' -> "Dash"
  '~' -> "Tilde"
  ';' -> "Semi"
  c -> error $ "opCharName: " <> show c

ident :: Id -> JS
ident i
  | T.all (`elem` varOpChars) i = "$" <> T.concatMap opCharName i
  | T.head i == '_' || T.head i == '$' = i
  | otherwise = "_" <> T.replace "'" "_" i

modName :: Id -> JS
modName i = "_" <> T.replace "." "_" (T.replace "'" "_" i)

name :: Name -> JS
name (Unqual i) = ident i
name (Qual m i) = modName m <> "." <> ident i

compileBindingGroup :: BindingGroup -> JS
compileBindingGroup = foldMap' compileBinding
  where
    compileBinding (False, bs) = foldMap' (\(i, v) -> "const " <> ident i <> "=" <> thunk v <> ";") bs
    compileBinding (True, bs) = decls <> assigns
      where
        decls = foldMap' ((\i -> "const " <> ident i <> "={};") . fst) bs
        assigns = foldMap' (\(i, v) -> ident i <> ".f=()=>" <> parenthesize v <> ";") bs

compileLet :: BindingGroup -> Expr -> JS
compileLet bs e = "(()=>{" <> compileBindingGroup bs <> "return " <> compileExpr e <> ";})()"

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

compileCase :: [Branch] -> JS
compileCase = foldr branch "(()=>{throw 'Incomplete pattern match';})()"
  where
    branch ([], e) _ = compileExpr e
    branch (cs, e) r = cond <> "?" <> compileExpr e <> ":" <> r
      where
        cond = T.intercalate "&&" $ (\(e', c) -> check (compileExpr e') c) <$> cs

compileExpr :: Expr -> JS
compileExpr (NumLit NaN) = "NaN"
compileExpr (NumLit Inf) = "Infinity"
compileExpr (NumLit NegInf) = "-Infinity"
compileExpr (NumLit (Num n)) = showT n
compileExpr (StrLit s) = showT s
compileExpr (RecLit fs) = "{" <> fields <> "}"
  where
    fields = T.intercalate "," $ (\(i, e) -> ident i <> ":" <> thunk e) <$> fs
compileExpr (CtorLit c es) = "{$:" <> showT c <> fields <> "}"
  where
    fields = foldMap' id $ imap (\i e -> ",$" <> showT i <> ":" <> thunk e) es
compileExpr (Var n) = "e(" <> name n <> ")"
compileExpr (Field e i) = "e(" <> compileExpr e <> "." <> ident i <> ")"
compileExpr (Case bs) = compileCase bs
compileExpr (Lam i e) = "(" <> ident i <> "=>" <> parenthesize e <> ")"
compileExpr (App f a) = compileExpr f <> "(" <> thunk a <> ")"
compileExpr (Let bs e) = compileLet bs e
compileExpr (UnsafeJS _ _ js) = js

isWhnf :: Expr -> Bool
isWhnf NumLit{} = True
isWhnf StrLit{} = True
isWhnf CtorLit{} = True
isWhnf RecLit{} = True
isWhnf Lam{} = True
isWhnf (UnsafeJS whnf _ _) = whnf
isWhnf _ = False

thunk :: Expr -> JS
thunk (Var n) = name n
thunk (Field e i) = compileExpr e <> "." <> ident i
thunk e
  | isWhnf e = "{v:" <> compileExpr e <> "}"
  | otherwise = "{f:()=>" <> parenthesize e <> "}"

parenthesize :: Expr -> JS
parenthesize e@UnsafeJS{} = compileExpr e
parenthesize (compileExpr -> js)
  | T.isPrefixOf "{" js = "(" <> js <> ")"
  | otherwise = js

compileModule :: Module -> JS
compileModule Module{..} =
  "const " <> modName moduleName <> "=" <> "(()=>{" <> moduleInitCode
  <> compileBindingGroup moduleBindings <> "return{"
  <> T.intercalate "," (ident <$> moduleExports) <> "}})();"

compile :: JS -> ([Module], Id) -> JS
compile prelude (ms, main) =
  prelude
  <> foldMap' compileModule ms
  <> compileExpr (Var $ Qual main "main")
  <> ".r();\n"
