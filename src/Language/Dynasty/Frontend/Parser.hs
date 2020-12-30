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

record :: Parser a -> Parser (Map Ident a)
record rhs = M.fromList <$> (unique =<< parens '{' '}' elems)
  where
    withTrailing = many (term <* comma)
    noTrailing = sepBy1 term comma
    elems = try withTrailing <|> noTrailing
    term = (,) <$> (varIdent <* equals) <*> (rhs <* ws)

    unique es =
      let es' = fst <$> es
      in
        if nub es' == es'
          then pure es
          else fail "Fields in a record must be unique"

list :: Parser (Expr a) -> Parser (Expr a)
list p = explode id <$> parens '[' ']' ((p <* ws) `sepEndBy` comma)

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

int :: Parser (Expr a)
int = NumLit <$> intRaw

char' :: Parser (Expr a)
char' = CharLit <$> charRaw

explode :: (a -> Expr k) -> [a] -> Expr k
explode _ [] = CtorLit "Nil" []
explode f (c : cs) = CtorLit "::" [f c, explode f cs]

str :: Parser (Expr a)
str = explode CharLit <$> strRaw

simpleLit :: Parser (Expr a)
simpleLit = try str <|> try char' <|> int

reservedNames :: [String]
reservedNames = ["let", "in", "match"]

reservedOps :: [String]
reservedOps = ["\\", "->", "|", "_", ":"]

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

var :: Parser (Expr a)
var = Var <$> varIdent

ctorSimple :: Parser (Expr a)
ctorSimple = (`CtorLit` []) <$> ctorIdent

tupLit :: Parser (Expr a) -> Parser (Expr a)
tupLit = tuple (CtorLit "Tuple")

recLit :: Parser (Expr a) -> Parser (Expr a)
recLit p = RecLit <$> record p

lam :: Parser (Expr 'E)
lam = mkLam <$> (char '\\' *> ws *> many1 (varIdent <* ws) <* char '.' <* ws) <*> expr
  where
    mkLam [] e = e
    mkLam (i : as) e = Lam i $ mkLam as e

let' :: Parser (Expr 'E)
let' =
  Let
  <$> (string "let" *> ws *> varIdent)
  <*> (equals *> expr)
  <*> (ws *> string "in" *> ws *> expr)

match :: Parser (Expr 'E)
match = Match <$> (string "match" *> ws *> expr <* ws) <*> many branch
  where
    branch = char '|' *> ws *> ((,) <$> (pat <* ws <* string "->" <* ws) <*> expr) <* ws

exprSimple :: Parser (Expr 'E)
exprSimple = choice (try <$> [match, lam, let', recLit expr, list expr, tupLit expr, simpleLit, ctorSimple, var]) <* ws

wildcard :: Parser (Expr 'P)
wildcard = char '_' $> Wildcard

ofType :: Parser (Expr 'P)
ofType = OfType <$> (varIdent <* colon) <*> pat

patSimple :: Parser (Expr 'P)
patSimple = choice (try <$> [ofType, wildcard, recLit pat, list pat, tupLit pat, simpleLit, ctorSimple, var]) <* ws

patCtorApp :: Parser (Expr 'P)
patCtorApp = try (CtorLit <$> (ctorIdent <* ws) <*> many (patSimple <* ws)) <|> patSimple

patCtorOps :: Parser (Expr 'P)
patCtorOps = chainl1 patCtorApp $ try $ appCtor <$> (ctorInfix <* ws)

pat :: Parser (Expr 'P)
pat = patCtorOps

member :: Parser (Expr 'E)
member = foldl' RecMember <$> exprSimple <*> try (many1 (char '.' *> varIdent))

exprMember :: Parser (Expr 'E)
exprMember = try member <|> exprSimple

appHead :: Parser ([Expr 'E] -> Expr 'E)
appHead = (try (CtorLit <$> ctorIdent) <|> foldl' App <$> exprMember) <* ws

exprApp :: Parser (Expr 'E)
exprApp = appHead <*> many (exprMember <* ws)

appCtor :: Ident -> Expr a -> Expr a -> Expr a
appCtor c a b = CtorLit c [a, b]

exprCtorOps :: Parser (Expr 'E)
exprCtorOps = chainl1 exprApp $ try $ appCtor <$> (ctorInfix <* ws)

appVar :: Ident -> Expr 'E -> Expr 'E -> Expr 'E
appVar v = App . App (Var v)

exprVarOps :: Parser (Expr 'E)
exprVarOps = chainr1 exprCtorOps $ appVar <$> (varInfix <* ws)

expr :: Parser (Expr 'E)
expr = exprVarOps

program :: Parser (Expr 'E)
program = option () shebang *> ws *> expr <* eof

parse :: MonadError String m => String -> m (Expr 'E)
parse = liftEither . first show . runParser program () ""
