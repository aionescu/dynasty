module Language.Dynasty.Parser(parse, varOpChars) where

import Control.Monad.Except(liftEither, MonadError)
import Data.Bifunctor(first)
import Data.Foldable(foldl')
import Data.Function(on)
import Data.Functor((<&>), ($>))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Vector(Vector)
import Data.Vector qualified as V
import Data.Void(Void)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import Language.Dynasty.Syntax
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)

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
reservedNames = ["case", "of", "let", "and", "in"]

reservedOps :: [Text]
reservedOps = ["=", "\\", "->", "|", ":"]

ident :: Parser Char -> Parser Text
ident fstChar = try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many sndChar) <?> "Identifier")
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
operator fstChar = try (notReserved . T.pack =<< lexeme ((:) <$> fstChar <*> many opChar) <?> "Operator")
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
varIdent = varName <|> parens (operator $ oneOf varOpChars)

ctorIdent :: Parser Text
ctorIdent = ctorName <|> parens (operator $ char ':')

tuple :: Parser (Syn k) -> Parser (Syn k)
tuple term = parens $ mkTup <$> (try term `sepBy` symbol ",")
  where
    mkTup [a] = a
    mkTup l = Tuple $ V.fromList l

recField :: Parser (Syn k) -> Parser (Ident, Maybe (Syn k))
recField p = (,) <$> varIdent <*> optional (try $ symbol "=" *> p) -- TODO: try may not be needed

record :: Parser (Ident, Maybe (Syn k)) -> Parser (Syn k)
record term = Record . V.fromList <$> btwn "{" "}" (term `sepBy` symbol ",")

recLit :: Parser (Syn k) -> Parser (Syn k)
recLit = record . recField

list :: Parser (Syn k) -> Parser (Syn k)
list term = List . V.fromList <$> btwn "[" "]" (term `sepBy` symbol ",")

intRaw :: Parser Integer
intRaw = L.signed (pure ()) $ lexeme L.decimal

charRaw :: Parser Char
charRaw = lexeme $ between (char '\'') (char '\'') L.charLiteral

strRaw :: Parser Text
strRaw = lexeme $ char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack

intLit :: Parser (Syn k)
intLit = Lit . Int <$> intRaw

charLit :: Parser (Syn k)
charLit = Lit . Char <$> charRaw

strLit :: Parser (Syn k)
strLit = Lit . Str <$> strRaw

simpleLit :: Parser (Syn k)
simpleLit = try intLit <|> charLit <|> strLit

var :: Parser (Syn k)
var = Var <$> varIdent

ctorSimple :: Parser (Syn k)
ctorSimple = (\i -> App (Ctor i) []) <$> ctorIdent

caseBranch :: Parser (Pat, Expr)
caseBranch = symbol "|" *> ((,) <$> (pat <* symbol "->" ) <*> expr)

lamCase :: Parser Expr
lamCase = symbol "case" *> many caseBranch <&> LambdaCase . V.fromList

lamVars :: Parser Expr
lamVars = Lambda . V.fromList <$> (some patSimple <* symbol ".") <*> expr

lam :: Parser Expr
lam = symbol "\\" *> (try lamCase <|> lamVars)

binding :: Parser (Ident, Expr)
binding = (,) <$> varIdent <*> (args <*> (symbol "=" *> expr))
  where
    args = (Lambda . V.fromList <$> some patSimple) <|> pure id

bindingGroup :: Parser BindingGroup
bindingGroup = V.fromList <$> try binding `sepBy1` symbol "and"

let' :: Parser Expr
let' =
  Let
  <$> try (symbol "let" *> bindingGroup)
  <*> (symbol "in" *> expr)

case' :: Parser Expr
case' = Case <$> (symbol "case" *> expr <* symbol "of") <*> (V.fromList <$> many caseBranch)

exprSimple :: Parser Expr
exprSimple = choice @[] $ try <$> [let', lam, case', recLit expr, list expr, tuple expr, simpleLit, ctorSimple, var]

wildcard :: Parser Pat
wildcard = symbol "_" $> Wildcard

ofType :: Parser Pat
ofType = OfType <$> (varIdent <* symbol ":") <*> pat

asPat :: Parser Pat
asPat = As <$> (varIdent <* symbol "@") <*> patSimple

patSimple :: Parser Pat
patSimple = choice @[] $ try <$> [asPat, ofType, wildcard, recLit pat, list pat, tuple pat, simpleLit, ctorSimple, var]

ctorHead :: Parser (Vector (Syn a) -> Syn a)
ctorHead = App . Ctor <$> ctorIdent

fnHead :: Parser (Vector Expr -> Expr)
fnHead = App . Fn <$> exprFieldAccess

patCtorApp :: Parser Pat
patCtorApp = try (ctorHead <*> (V.fromList <$> some patSimple)) <|> patSimple

patOps :: [[Operator Parser Pat]]
patOps =
  [ [ inf R opCtor ]
  , [ inf R opInfixCtor ]
  ]

pat :: Parser Pat
pat = makeExprParser patCtorApp patOps

fieldAccess :: Parser Expr
fieldAccess = foldl' FieldAccess <$> exprSimple <*> try (some (char '.' *> varIdent))

exprFieldAccess :: Parser Expr
exprFieldAccess = try fieldAccess <|> exprSimple

appHead :: Parser (Vector Expr -> Expr)
appHead = try ctorHead <|> fnHead

exprApp :: Parser Expr
exprApp = try (appHead <*> (V.fromList <$> some exprFieldAccess)) <|> exprFieldAccess

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

parse :: MonadError Text m => Text -> m BindingGroup
parse = liftEither . first (T.pack . errorBundlePretty) . runParser program ""
