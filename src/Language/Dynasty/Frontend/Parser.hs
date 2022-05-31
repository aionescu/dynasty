module Language.Dynasty.Frontend.Parser where

import Control.Monad.Except(liftEither, MonadError)
import Data.Bifunctor(first)
import Data.Char(isUpper, isLower)
import Data.Containers.ListUtils(nubOrd)
import Data.Foldable(foldl')
import Data.Function(on)
import Data.Functor((<&>), ($>))
import Data.Map.Strict qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.Dynasty.Frontend.Syntax

type Parser = Parsec Void Text

lineComm :: Parser ()
lineComm = L.skipLineComment "--"

blockComm :: Parser ()
blockComm = L.skipBlockCommentNested "{-" "-}"

shebang :: Parser ()
shebang = L.skipLineComment "#!" <* newline

sc :: Parser ()
sc = L.space space1 lineComm blockComm

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

btwn :: Text -> Text -> Parser a -> Parser a
btwn = between `on` symbol

parens :: Parser a -> Parser a
parens = btwn "(" ")"

reservedNames :: [String]
reservedNames = ["case", "of", "let", "and", "in"]

reservedOps :: [String]
reservedOps = ["=", "\\", "->", "|", ":"]

identRaw :: Parser String
identRaw = notReserved =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier"
  where
    fstChar = letterChar
    sndChar = alphaNumChar <|> char '\''

    notReserved i
      | i `elem` reservedNames = fail $ "Reserved name " <> show i
      | otherwise = pure i

opRaw :: Parser String
opRaw = notReserved =<< lexeme (some opChar) <?> "Operator"
  where
    opChar = oneOf @[] "!#$%&*+./<=>?@\\^|-~:;_"

    notReserved i
      | i `elem` reservedOps = fail $ "Reserved operator " <> show i
      | otherwise = pure i

varName :: Parser Text
varName = identRaw >>= \case
  (c : _) | isUpper c -> fail "Variable names must begin with a lowercase letter."
  i -> pure $ T.pack i

ctorName :: Parser Text
ctorName = identRaw >>= \case
  (c : _) | isLower c -> fail "Constructor names must begin with an uppercase letter."
  i -> pure $ T.pack i

varOp :: Parser Text
varOp = opRaw >>= \case
  (':' : _) -> fail "Variable operators cannot begin with ':'."
  i -> pure $ T.pack i

ctorOp :: Parser Text
ctorOp = opRaw >>= \case
  (c : _) | c /= ':' -> fail "Constructor operators must begin with ':'."
  i -> pure $ T.pack i

varIdent :: Parser Text
varIdent = try varName <|> parens varOp

ctorIdent :: Parser Text
ctorIdent = try ctorName <|> parens ctorOp

varInfix :: Parser Text
varInfix = try varOp <|> btwn "`" "`" varName

ctorInfix :: Parser Text
ctorInfix = try ctorOp <|> btwn "`" "`" ctorName

tuple :: Parser (Node k) -> Parser (Node k)
tuple term = parens $ mkTup <$> (term `sepBy` symbol ",")
  where
    mkTup [a] = a
    mkTup l = CtorLit "Tuple" $ V.fromList l

recField :: Parser (Node k) -> Parser (Ident, Node k)
recField p = mkField <$> varIdent <*> optional (try $ symbol "=" *> p) -- TODO: try may not be needed
  where
    mkField i Nothing = (i, Var i)
    mkField i (Just n) = (i, n)

record :: Parser (Ident, Node k) -> Parser (Node k)
record term = btwn "{" "}" (term `sepBy` symbol ",") >>= unique <&> RecLit
  where
    unique es =
      let es' = fst <$> es
      in
        if nubOrd es' == es'
        then pure $ M.fromList es
        else fail "Fields in a record must be unique"

recLit :: Parser (Node k) -> Parser (Node k)
recLit = record . recField

list :: ([a] -> b) -> Parser a -> Parser b
list f p = f <$> btwn "[" "]" (p `sepBy` symbol ",")

explode :: (a -> Node k) -> [a] -> Node k
explode _ [] = CtorLit "Nil" []
explode f (c : cs) = CtorLit "::" [f c, explode f cs]

listLit :: Parser (Node k) -> Parser (Node k)
listLit = list $ explode id

intRaw :: Parser Integer
intRaw = L.signed (pure ()) $ lexeme L.decimal

charRaw :: Parser Char
charRaw = lexeme $ between (char '\'') (char '\'') L.charLiteral

strRaw :: Parser Text
strRaw = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack

intLit :: Parser (Node k)
intLit = NumLit <$> intRaw

charLit :: Parser (Node k)
charLit = CharLit <$> charRaw

strLit :: Parser (Node k)
strLit = StrLit <$> strRaw

simpleLit :: Parser (Node k)
simpleLit = try intLit <|> charLit <|> strLit

var :: Parser (Node k)
var = Var <$> varIdent

ctorSimple :: Parser (Node k)
ctorSimple = (`CtorLit` []) <$> ctorIdent

mkLam :: [Pat] -> Expr -> Expr
mkLam = flip $ foldr \p e ->
  case p of
    Var i -> Lam i e
    _ -> LamCase [(p, e)]

caseBranch :: Parser (Pat, Expr)
caseBranch = symbol "|" *> ((,) <$> (pat <* symbol "->" ) <*> expr)

lamCase :: Parser Expr
lamCase = symbol "case" *> many caseBranch <&> LamCase . V.fromList

lamVars :: Parser Expr
lamVars = mkLam <$> (some patSimple <* symbol ".") <*> expr

lam :: Parser Expr
lam = symbol "\\" *> (try lamCase <|> lamVars)

binding :: Parser (Ident, Expr)
binding = (,) <$> varIdent <*> (mkLam <$> many patSimple <*> (symbol "=" *> expr))

bindingGroup :: Parser BindingGroup
bindingGroup = try binding `sepBy1` symbol "and" >>= ensureUnique
  where
    ensureUnique bs =
      let is = fst <$> bs
      in
        if is == nubOrd is
        then pure $ M.fromList bs
        else fail "Duplicate definition in binding group"

let' :: Parser Expr
let' =
  Let
  <$> try (symbol "let" *> bindingGroup)
  <*> (symbol "in" *> expr)

case' :: Parser Expr
case' = Case <$> (symbol "case" *> expr <* symbol "of") <*> (V.fromList <$> many caseBranch)

exprSimple :: Parser Expr
exprSimple = choice @[] $ try <$> [let', lam, case', recLit expr, listLit expr, tuple expr, simpleLit, ctorSimple, var]

wildcard :: Parser Pat
wildcard = symbol "_" $> Wildcard

ofType :: Parser Pat
ofType = OfType <$> (varIdent <* symbol ":") <*> pat

asPat :: Parser Pat
asPat = As <$> (varIdent <* symbol "@") <*> patSimple

patSimple :: Parser Pat
patSimple = choice @[] $ try <$> [asPat, ofType, wildcard, recLit pat, listLit pat, tuple pat, simpleLit, ctorSimple, var]

patCtorApp :: Parser Pat
patCtorApp = try (CtorLit <$> ctorIdent <*> (V.fromList <$> many patSimple)) <|> patSimple

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 term op = apply <$> term <*> many ((,) <$> op <*> term)
  where
    apply = foldl' (\a (f, b) -> f a b)

appCtor :: Ident -> Node k -> Node k -> Node k
appCtor c a b = CtorLit c [a, b]

patCtorOps :: Parser Pat
patCtorOps = chainl1 patCtorApp $ try $ appCtor <$> ctorInfix

pat :: Parser Pat
pat = patCtorOps

member :: Parser Expr
member = foldl' RecMember <$> exprSimple <*> try (some (char '.' *> varIdent))

exprMember :: Parser Expr
exprMember = try member <|> exprSimple

appHead :: Parser (Vector Expr -> Expr)
appHead = try (CtorLit <$> ctorIdent) <|> foldl' App <$> exprMember

exprApp :: Parser Expr
exprApp = appHead <*> (V.fromList <$> many exprMember)

exprCtorOps :: Parser Expr
exprCtorOps = chainl1 exprApp $ try $ appCtor <$> ctorInfix

appVar :: Ident -> Expr -> Expr -> Expr
appVar v = App . App (Var v)

exprVarOps :: Parser Expr
exprVarOps = chainl1 exprCtorOps $ appVar <$> varInfix

expr :: Parser Expr
expr = exprVarOps

program :: Parser BindingGroup
program = optional shebang *> optional sc *> bindingGroup <* eof

parse :: MonadError Text m => Text -> m BindingGroup
parse = liftEither . first (T.pack . errorBundlePretty) . runParser program ""
