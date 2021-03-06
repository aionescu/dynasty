module Language.Dynasty.Frontend.Parser where

import Data.Bifunctor(first)
import Data.Char(isUpper, isLower)
import Data.Functor((<&>), ($>))
import Data.List(foldl', nub)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Control.Monad.Except(liftEither, MonadError)
import Text.Parsec hiding (parse)

import Language.Dynasty.Frontend.Syntax

type Parser = Parsec String ()

comma, colon, equals, shebang, multiLine, singleLine, comment, ws :: Parser ()
comma = ws <* char ',' <* ws
colon = ws <* char ':' <* ws
equals = ws <* char '=' <* ws
shebang = try $ string "#!" *> manyTill anyChar (endOfLine $> ()) $> ()
multiLine = try $ string "{-" *> manyTill (multiLine <|> (anyChar $> ())) (try $ string "-}") $> ()
singleLine = try $ string "--" *> manyTill anyChar (eof <|> endOfLine $> ()) $> ()
comment = singleLine <|> multiLine
ws = spaces *> skipMany (comment *> spaces)

parens :: Char -> Char -> Parser a -> Parser a
parens begin end = between (char begin *> ws) (char end *> ws)

tuple :: ([a] -> a) -> Parser a -> Parser a
tuple mk term = parens '(' ')' $ uncurry mkTup <$> elems
  where
    withTrailing = (, True) <$> many (term <* comma)
    noTrailing = (, False) <$> sepBy1 term comma
    elems = try withTrailing <|> noTrailing

    mkTup [a] False = a
    mkTup l _ = mk l

recField :: Parser (Node a) -> Parser (Ident, Node a)
recField p = mkField <$> (varIdent <* ws) <*> optionMaybe (try $ equals *> p)
  where
    mkField i Nothing = (i, Var i)
    mkField i (Just n) = (i, n)

record :: Parser (Ident, f) -> (Map Ident f -> Bool -> Parser a) -> Parser a
record term f = parens '{' '}' elems >>= unique >>= uncurry f
  where
    withTrailing = (,) <$> many (term <* comma) <*> option False (string ".." *> ws $> True)
    noTrailing = (, False) <$> sepBy1 term comma
    elems = try withTrailing <|> noTrailing

    unique (es, b) =
      let es' = fst <$> es
      in
        if nub es' == es'
        then pure (M.fromList es, b)
        else fail "Fields in a record must be unique"

recLit :: Parser (Node k) -> (Map Ident (Node k) -> Bool -> Parser a) -> Parser a
recLit = record . recField

list :: ([a] -> b) -> Parser a -> Parser b
list f p = f <$> parens '[' ']' ((p <* ws) `sepEndBy` comma)

listLit :: Parser (Node a) -> Parser (Node a)
listLit = list $ explode id

number :: (Read a, Num a) => Parser a
number = read <$> many1 digit

intRaw :: Parser Integer
intRaw = sign <*> number
  where
    sign = option id (char '-' $> negate)

escaped :: Char -> Parser Char
escaped quote = regular <|> unescaped
  where
    unescape '\\' = '\\'
    unescape '0' = '\0'
    unescape 'n' = '\n'
    unescape 'r' = '\r'
    unescape 'v' = '\v'
    unescape 't' = '\t'
    unescape 'b' = '\b'
    unescape 'f' = '\f'
    unescape a = a

    unescaped = char '\\' *> oneOf (quote : "\\0nrvtbf") <&> unescape
    regular = noneOf $ quote : "\\\0\n\r\v\t\b\f"

charRaw :: Parser Char
charRaw = parens '\'' '\'' $ escaped '\''

strRaw :: Parser String
strRaw = parens '"' '"' $ many $ escaped '"'

int :: Parser (Node a)
int = NumLit <$> intRaw

char' :: Parser (Node a)
char' = CharLit <$> charRaw

explode :: (a -> Node k) -> [a] -> Node k
explode _ [] = CtorLit "Nil" []
explode f (c : cs) = CtorLit "::" [f c, explode f cs]

str :: Parser (Node a)
str = explode CharLit <$> strRaw

simpleLit :: Parser (Node a)
simpleLit = try str <|> try char' <|> int

reservedNames :: [String]
reservedNames = ["case", "of", "let", "and", "in"]

reservedOps :: [String]
reservedOps = ["=", "\\", "->", "|", ":"]

identRaw :: Parser String
identRaw = notReserved =<< (:) <$> fstChar <*> many sndChar
  where
    fstChar = letter
    sndChar = choice [letter, digit, char '\'']

    notReserved i
      | i `elem` reservedNames = fail "Reserved name"
      | otherwise = pure i

opRaw :: Parser String
opRaw = notReserved =<< many1 opChar
  where
    opChar = oneOf "!#$%&*+./<=>?@\\^|-~:;_"

    notReserved i
      | i `elem` reservedOps = fail "Reserved operator"
      | otherwise = pure i

varName :: Parser String
varName = identRaw >>= \case
  (c : _) | isUpper c -> fail "Variable names must begin with a lowercase letter."
  i -> pure i

ctorName :: Parser String
ctorName = identRaw >>= \case
  (c : _) | isLower c -> fail "Constructor names must begin with an uppercase letter."
  i -> pure i

varOp :: Parser String
varOp = opRaw >>= \case
  (':' : _) -> fail "Variable operators cannot begin with ':'."
  i -> pure i

ctorOp :: Parser String
ctorOp = opRaw >>= \case
  (c : _) | c /= ':' -> fail "Constructor operators must begin with ':'."
  i -> pure i

varIdent :: Parser String
varIdent = try varName <|> parens '(' ')' varOp

ctorIdent :: Parser String
ctorIdent = try ctorName <|> parens '(' ')' ctorOp

varInfix :: Parser String
varInfix = try varOp <|> parens '`' '`' varName

ctorInfix :: Parser String
ctorInfix = try ctorOp <|> parens '`' '`' ctorName

var :: Parser (Node a)
var = Var <$> varIdent

ctorSimple :: Parser (Node a)
ctorSimple = (`CtorLit` []) <$> ctorIdent

tupLit :: Parser (Node a) -> Parser (Node a)
tupLit = tuple (CtorLit "Tuple")

recLitExpr :: Parser Expr
recLitExpr = recLit expr \m b ->
  if b
  then fail "Record wildcards can only appear in patterns"
  else pure $ RecLit m

recLitPat :: Parser Pat
recLitPat = recLit pat \m b -> pure $ (if b then RecWildcard else RecLit) m

mkLam :: [Pat] -> Expr -> Expr
mkLam = flip $ foldr \p e -> Lam [(p, e)]

caseBranch :: Parser (Pat, Expr)
caseBranch = char '|' *> ws *> ((,) <$> (pat <* ws <* string "->" <* ws) <*> expr) <* ws

lamCase :: Parser Expr
lamCase = string "case" *> ws *> many (caseBranch <* ws) <&> Lam

lamVars :: Parser Expr
lamVars = mkLam <$> (many1 (patSimple <* ws) <* char '.' <* ws) <*> expr

lam :: Parser Expr
lam = char '\\' *> ws *> (try lamCase <|> lamVars)

binding :: Parser (Ident, Expr)
binding = (,) <$> (varIdent <* ws) <*> (mkLam <$> many (patSimple <* ws) <*> (equals *> expr <* ws))

bindingGroup :: Parser BindingGroup
bindingGroup = try binding `sepBy1` try (string "and" *> ws) >>= ensureUnique
  where
    ensureUnique bs =
      let is = fst <$> bs
      in
        if is == nub is
        then pure $ M.fromList bs
        else fail "Duplicate definition in binding group"

let' :: Parser Expr
let' =
  Let
  <$> try (string "let" *> ws *> bindingGroup)
  <*> (string "in" *> ws *> expr)

case' :: Parser Expr
case' = mkCase <$> (string "case" *> ws *> expr <* ws <* string "of" <* ws) <*> many caseBranch
  where
    mkCase e bs = App (Lam bs) e

exprSimple :: Parser Expr
exprSimple = choice (try <$> [let', lam, case', recLitExpr, listLit expr, tupLit expr, simpleLit, ctorSimple, var]) <* ws

wildcard :: Parser Pat
wildcard = char '_' $> Wildcard

ofType :: Parser Pat
ofType = OfType <$> (varIdent <* colon) <*> pat

asPat :: Parser Pat
asPat = As <$> (varIdent <* char '@') <*> patSimple

patSimple :: Parser Pat
patSimple = choice (try <$> [asPat, ofType, wildcard, recLitPat, listLit pat, tupLit pat, simpleLit, ctorSimple, var]) <* ws

patCtorApp :: Parser Pat
patCtorApp = try (CtorLit <$> (ctorIdent <* ws) <*> many (patSimple <* ws)) <|> patSimple

patCtorOps :: Parser Pat
patCtorOps = chainl1 patCtorApp $ try $ appCtor <$> (ctorInfix <* ws)

pat :: Parser Pat
pat = patCtorOps

member :: Parser Expr
member = foldl' RecMember <$> exprSimple <*> try (many1 (char '.' *> varIdent))

exprMember :: Parser Expr
exprMember = try member <|> exprSimple

appHead :: Parser ([Expr] -> Expr)
appHead = (try (CtorLit <$> ctorIdent) <|> foldl' App <$> exprMember) <* ws

exprApp :: Parser Expr
exprApp = appHead <*> many (exprMember <* ws)

appCtor :: Ident -> Node a -> Node a -> Node a
appCtor c a b = CtorLit c [a, b]

exprCtorOps :: Parser Expr
exprCtorOps = chainl1 exprApp $ try $ appCtor <$> (ctorInfix <* ws)

appVar :: Ident -> Expr -> Expr -> Expr
appVar v = App . App (Var v)

exprVarOps :: Parser Expr
exprVarOps = chainl1 exprCtorOps $ appVar <$> (varInfix <* ws)

expr :: Parser Expr
expr = exprVarOps

program :: Parser BindingGroup
program = option () shebang *> ws *> bindingGroup <* eof

parse :: MonadError String m => String -> m BindingGroup
parse = liftEither . first show . runParser program () ""
