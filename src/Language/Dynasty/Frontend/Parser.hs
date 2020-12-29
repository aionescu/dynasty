module Language.Dynasty.Frontend.Parser where

import Data.Bifunctor(first)
import Data.Char(isUpper, isLower)
import Data.Functor((<&>), ($>))
import Data.List(foldl', nub)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
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

list :: Parser Expr
list = explode id <$> parens '[' ']' ((expr <* ws) `sepEndBy` comma)

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

int :: Parser Expr
int = NumLit <$> intRaw

char' :: Parser Expr
char' = CharLit <$> charRaw

explode :: (a -> Expr) -> [a] -> Expr
explode _ [] = CtorLit "Nil" []
explode f (c : cs) = CtorLit "::" [f c, explode f cs]

str :: Parser Expr
str = explode CharLit <$> strRaw

simpleLit :: Parser Expr
simpleLit = try str <|> try char' <|> int

reservedNames :: [String]
reservedNames = ["let", "in", "if", "then", "else"]

reservedOps :: [String]
reservedOps = ["->"]

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
    opChar = oneOf "!#$%&*+./<=>?@\\^|-~:;"

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

var :: Parser Expr
var = Var <$> varIdent

ctorSimple :: Parser Expr
ctorSimple = (`CtorLit` []) <$> ctorIdent

tupLit :: Parser Expr
tupLit = tuple (CtorLit "Tuple") expr

recLit :: Parser Expr
recLit = RecLit <$> record expr

if' :: Parser Expr
if' =
  If
  <$> (string "if" *> ws *> expr <* ws)
  <*> (string "then" *> ws *> expr <* ws)
  <*> (string "else" *> ws *> expr <* ws)

lam :: Parser Expr
lam = mkLam <$> (many1 (varIdent <* ws) <* string "->" <* ws) <*> expr
  where
    mkLam [] e = e
    mkLam (i : as) e = Lam i $ mkLam as e

let' :: Parser Expr
let' =
  Let
  <$> (string "let" *> ws *> varIdent)
  <*> (equals *> expr)
  <*> (ws *> string "in" *> ws *> expr)

exprSimple :: Parser Expr
exprSimple = choice (try <$> [lam, let', if', recLit, list, tupLit, simpleLit, ctorSimple, var]) <* ws

exprMember :: Parser Expr
exprMember = foldl' RecMember <$> exprSimple <*> many (char '.' *> varIdent)

appHead :: Parser ([Expr] -> Expr)
appHead = (try (CtorLit <$> ctorIdent) <|> foldl' App <$> exprMember) <* ws

exprApp :: Parser Expr
exprApp = appHead <*> many (exprMember <* ws)

appCtor :: Ident -> Expr -> Expr -> Expr
appCtor c a b = CtorLit c [a, b]

exprCtorOps :: Parser Expr
exprCtorOps = chainl1 exprApp $ try $ appCtor <$> (ctorInfix <* ws)

appVar :: Ident -> Expr -> Expr -> Expr
appVar v = App . App (Var v)

exprVarOps :: Parser Expr
exprVarOps = chainr1 exprCtorOps $ appVar <$> (varInfix <* ws)

expr :: Parser Expr
expr = exprVarOps

program :: Parser Expr
program = option () shebang *> ws *> expr <* eof

parse :: MonadError String m => String -> m Expr
parse = liftEither . first show . runParser program () ""
