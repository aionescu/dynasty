module Language.Dynasty.Codegen(compile) where

import Data.Foldable(foldMap')
import Data.Functor((<&>))
import Data.Set(Set)
import Data.Set qualified as S
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
compileCase = foldr branch "i()"
  where
    branch ([], e) _ = compileExpr e
    branch (cs, e) r = cond <> "?" <> compileExpr e <> ":" <> r
      where
        cond = T.intercalate "&&" $ (\(e', c) -> check (compileExpr e') c) <$> cs

compileCaseStmt :: [Branch] -> JS
compileCaseStmt = foldr branch "i();"
  where
    branch (cs, e) r
      | [] <- cs = ifBranch e
      | otherwise = "if(" <> cond <> ")" <> ifBranch e <> "else " <> r
      where
        cond = T.intercalate "&&" $ (\(e', c) -> check (compileExpr e') c) <$> cs
        ifBranch e
          | isStmt e = "{" <> stmt e <> "}"
          | otherwise = "return " <> compileExpr e <> ";"

ret :: Bool -> JS -> JS
ret False js = "return " <> js <> ";"
ret True js = js

retExpr :: Set Id -> Expr -> JS
retExpr d (Let i v e) = ret s $ compileLet d s i v e
  where
    s = S.notMember i d
retExpr d (LetRec bs e) = ret s $ compileLetRec d s bs e
  where
    s = all (`S.notMember` d) (fst <$> bs)
retExpr _ e
  | isStmt e = stmt e
  | otherwise = "return " <> compileExpr e <> ";"

compileLet :: Set Id -> Bool -> Id -> Expr -> Expr -> JS
compileLet d s i v e =
  wrap $ "const " <> ident i <> "=" <> thunk v <> ";" <> retExpr (S.insert i d) e
  where
    wrap js
      | s = js
      | otherwise = "(()=>{" <> js <> "})()"

compileLetRec :: Set Id -> Bool -> [(Id, Expr)] -> Expr -> JS
compileLetRec d s bs e =
  wrap $ decls <> assigns <> retExpr (S.fromList (fst <$> bs) <> d) e
  where
    decls = "const " <> T.intercalate "," (bs <&> \(i, _) -> ident i <> "={}") <> ";"
    assigns = foldMap' (\(i, v) -> ident i <> thunkAssign v <> ";") bs
    wrap js
      | s = js
      | otherwise = "(()=>{" <> js <> "})()"

compileExpr :: Expr -> JS
compileExpr (NumLit NaN) = "NaN"
compileExpr (NumLit Inf) = "Infinity"
compileExpr (NumLit NegInf) = "-Infinity"
compileExpr (NumLit (Num n)) = showT n
compileExpr (StrLit s) = showT s
compileExpr (RecLit fs) = "{" <> fields <> "}"
  where
    fields = T.intercalate "," $ fs <&> \case
      (i, Var (Unqual v)) | i == v -> ident i
      (i, e) -> ident i <> ":" <> thunk e

compileExpr (CtorLit c es) = "{$:" <> showT c <> fields <> "}"
  where
    fields = foldMap' id $ imap (\i e -> ",$" <> showT i <> ":" <> thunk e) es
compileExpr (Var n) = "e(" <> name n <> ")"
compileExpr (Field e i) = "e(" <> parenthesizeLam e <> "." <> ident i <> ")"
compileExpr (Case bs) = compileCase bs
compileExpr (Lam i e) = ident i <> "=>" <> fnBody e
compileExpr (App f a) = parenthesizeLam f <> "(" <> thunk a <> ")"
compileExpr (Let i v e) = compileLet S.empty False i v e
compileExpr (LetRec bs e) = compileLetRec S.empty False bs e
compileExpr (UnsafeJS _ js) = js

fnBody :: Expr -> JS
fnBody e
  | isStmt e = "{" <> stmt e <> "}"
  | otherwise = parenthesize e

isWhnf :: Expr -> Bool
isWhnf NumLit{} = True
isWhnf StrLit{} = True
isWhnf CtorLit{} = True
isWhnf RecLit{} = True
isWhnf Lam{} = True
isWhnf _ = False

isStmt :: Expr -> Bool
isStmt (Case bs) = any isStmt $ snd <$> bs
isStmt Let{} = True
isStmt LetRec{} = True
isStmt _ = False

stmt :: Expr -> JS
stmt (Case bs) = compileCaseStmt bs
stmt (Let i v e) = compileLet S.empty True i v e
stmt (LetRec bs e) = compileLetRec S.empty True bs e
stmt e = compileExpr e

thunk :: Expr -> JS
thunk (UnsafeJS _ js) = js
thunk (Var n) = name n
thunk (Field e i) = parenthesizeLam e <> "." <> ident i
thunk e
  | isWhnf e = "{v:" <> compileExpr e <> "}"
  | otherwise = "{f:()=>" <> fnBody e <> "}"

thunkAssign :: Expr -> JS
thunkAssign e
  | isWhnf e = ".v=" <> compileExpr e
  | otherwise = ".f=()=>" <> fnBody e

parenthesizeLam :: Expr -> JS
parenthesizeLam e@Lam{} = "(" <> compileExpr e <> ")"
parenthesizeLam e@Case{} = "(" <> compileExpr e <> ")"
parenthesizeLam e = compileExpr e

parenthesize :: Expr -> JS
parenthesize e@UnsafeJS{} = compileExpr e
parenthesize (compileExpr -> js)
  | T.isPrefixOf "{" js = "(" <> js <> ")"
  | otherwise = js

compileModule :: Module -> JS
compileModule Module{..} = modName moduleName <> "=" <> compileExpr moduleBody

compile :: JS -> ([Module], Id) -> JS
compile prelude (ms, main) =
  prelude
  <> "const " <> T.intercalate "," (compileModule <$> ms) <> ";"
  <> compileExpr (Var $ Qual main "main")
  <> ".r()\n"
