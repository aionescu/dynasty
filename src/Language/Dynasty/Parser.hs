module Language.Dynasty.Parser(parse, varOpChars) where

import Control.Monad.Combinators.Expr(Operator (..), makeExprParser)
import Control.Monad.Except(liftEither, MonadError)
import Data.Bifunctor(first)
import Data.Foldable(foldl')
import Data.Function(on, (&))
import Data.Functor((<&>), ($>))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.Dynasty.Syntax

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

reservedNames :: [Text]
reservedNames = ["case", "of", "as", "let", "and", "in", "NaN", "Infinity"]

reservedOps :: [Text]
reservedOps = ["=", "\\", "->", "|"]

ident :: Parser Char -> Parser Text
ident fstChar =
  try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier")
  where
    sndChar = alphaNumChar <|> char '\''

    notReserved i
      | i `elem` reservedNames = fail $ "Reserved name " <> show i
      | otherwise = pure i

varOpChars :: [Char]
varOpChars = "!#$%&*+./<=>?@\\^|-~;"

opChars :: [Char]
opChars = ':' : varOpChars

operator :: Parser Char -> Parser Text
operator fstChar =
  try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many opChar) <?> "Operator")
  where
    opChar = oneOf opChars

    notReserved i
      | i `elem` reservedOps = fail $ "Reserved operator " <> show i
      | otherwise = pure i

varName :: Parser Text
varName = ident lowerChar

ctorName :: Parser Text
ctorName = ident upperChar

data Fixity = L | R

inf :: Fixity -> Parser (a -> a -> a) -> Operator Parser a
inf L = InfixL
inf R = InfixR

opE :: Parser Char -> Parser (Expr -> Expr -> Expr)
opE c = operator c <&> \o a b -> App (Fn $ Var o) [a, b]

opCtor :: Parser (Syn a -> Syn a -> Syn a)
opCtor = operator (char ':') <&> \o a b -> App (Ctor o) [a, b]

opInfixE :: Parser (Expr -> Expr -> Expr)
opInfixE = between (char '`') (char '`') varName <&> \o a b -> App (Fn $ Var o) [a, b]

opInfixCtor :: Parser (Syn a -> Syn a -> Syn a)
opInfixCtor = between (char '`') (char '`') ctorName <&> \o a b -> App (Ctor o) [a, b]

varIdent :: Parser Text
varIdent = varName <|> try (parens $ operator $ oneOf varOpChars)

ctorIdent :: Parser Text
ctorIdent = ctorName <|> try (parens $ operator $ char ':')

tuple :: Parser (Syn k) -> Parser (Syn k)
tuple term = parens $ mkTup <$> (try term `sepBy` symbol ",")
  where
    mkTup [a] = a
    mkTup l = Tuple l

record :: Parser (Syn k) -> Parser (Syn k)
record term =
  Record <$> btwn "{" "}" (field `sepBy` symbol ",") <?> "Record literal"
  where
    field = (,) <$> varIdent <*> optional (symbol "=" *> term)

list :: Parser (Syn k) -> Parser (Syn k)
list term = List <$> btwn "[" "]" (term `sepBy` symbol ",") <?> "List literal"

signed :: Num a => Parser (a -> a)
signed = (char '-' $> negate) <|> pure id

numLit :: Parser (Syn a)
numLit =
  choice
  [ NumLit NaN <$ symbol "NaN"
  , NumLit Inf <$ symbol "Infinity"
  , NumLit NegInf <$ symbol "-Infinity"
  , NumLit . Num <$> (signed <*> lexeme L.scientific)
  ]
  <?> "Integer literal"

strRaw :: Parser Text
strRaw = lexeme (char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack) <?> "String literal"

strLit :: Parser (Syn k)
strLit = StrLit <$> strRaw

simpleLit :: Parser (Syn k)
simpleLit = try numLit <|> strLit

var :: Parser (Syn k)
var = Var <$> varIdent

ctorSimple :: Parser (Syn k)
ctorSimple = (\i -> App (Ctor i) []) <$> ctorIdent

caseBranch :: Parser (Pat, Expr)
caseBranch = symbol "|" *> ((,) <$> (pat <* symbol "->" ) <*> expr)

lamCase :: Parser Expr
lamCase = symbol "case" *> (LambdaCase <$> many caseBranch)

lamVars :: Parser Expr
lamVars = Lambda <$> (some patSimple <* symbol ".") <*> expr

lam :: Parser Expr
lam = symbol "\\" *> (try lamCase <|> lamVars)

binding :: Parser (Ident, Expr)
binding = (,) <$> varIdent <*> (args <*> (symbol "=" *> expr))
  where
    args = (Lambda <$> some patSimple) <|> pure id

bindingGroup :: Parser BindingGroup
bindingGroup = try binding `sepBy1` symbol "and"

let' :: Parser Expr
let' =
  Let
  <$> (symbol "let" *> bindingGroup)
  <*> (symbol "in" *> expr)

case' :: Parser Expr
case' = Case <$> (symbol "case" *> expr <* symbol "of") <*> many caseBranch

exprSimple :: Parser Expr
exprSimple =
  choice
  [ctorSimple, var, tuple expr, let', lam, case', record expr, list expr, simpleLit]
  <?> "Expression"

wildcard :: Parser Pat
wildcard = symbol "_" $> Wildcard

patSimple :: Parser Pat
patSimple =
  choice
  [ctorSimple, var, tuple pat, record pat, list pat, simpleLit, wildcard]
  <?> "Pattern"

ctorHead :: Parser ([Syn a] -> Syn a)
ctorHead = App . Ctor <$> ctorIdent

fnHead :: Parser ([Expr] -> Expr)
fnHead = App . Fn <$> exprField

asPat :: Parser Pat
asPat = as <$> patSimple <*> optional (try $ symbol "as" *> varIdent)
  where
    as p Nothing = p
    as p (Just v) = As p v

patCtorApp :: Parser Pat
patCtorApp = try (ctorHead <*> some asPat) <|> asPat

patOps :: [[Operator Parser Pat]]
patOps =
  [ [ inf R opCtor ]
  , [ inf R opInfixCtor ]
  ]

pat :: Parser Pat
pat = makeExprParser patCtorApp patOps

exprField :: Parser Expr
exprField = foldl' (&) <$> exprSimple <*> many (try $ char '.' *> field)
  where
    field = (flip CtorField <$> lexeme L.decimal) <|> (flip RecordField <$> varIdent)

appHead :: Parser ([Expr] -> Expr)
appHead = try ctorHead <|> fnHead

exprApp :: Parser Expr
exprApp = appHead <*> many exprField

exprOps :: [[Operator Parser Expr]]
exprOps =
  [ [ inf R $ opE $ char '^' ]
  , [ inf L $ opE $ oneOf @[] "*/%" ]
  , [ inf L $ opE $ oneOf @[] "+-&" ]
  , [ inf R $ opE $ char ';' ]
  , [ inf R $ opE $ oneOf @[] "@." ]
  , [ inf R $ opE $ oneOf @[] "<>" ]
  , [ inf L $ opE $ oneOf @[] "=!|~" ]
  , [ inf R $ opE $ oneOf @[] "$@\\?" ]
  , [ inf R opCtor ]
  , [ inf L opInfixE ]
  , [ inf R opInfixCtor ]
  ]

expr :: Parser Expr
expr = makeExprParser exprApp exprOps

program :: Parser BindingGroup
program = optional shebang *> optional sc *> bindingGroup <* eof

parse :: MonadError Text m => FilePath -> Text -> m BindingGroup
parse path = liftEither . first (T.pack . errorBundlePretty) . runParser program path
