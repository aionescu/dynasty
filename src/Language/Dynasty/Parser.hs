module Language.Dynasty.Parser(parse, varOpChars) where

import Control.Monad.Combinators.Expr(Operator(..), makeExprParser)
import Data.Bifunctor(first)
import Data.Foldable(foldl')
import Data.Function(on, (&))
import Data.Functor((<&>), ($>))
import Data.List(foldl1')
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

reservedNames :: [Text]
reservedNames =
  [ "unsafejs", "unsafejswhnf"
  , "module", "import"
  , "case", "of"
  , "let", "and", "in"
  , "NaN", "Infinity"
  , "do", "then"
  ]

reservedOps :: [Text]
reservedOps = ["=", "\\", "->", "<-", "|", "@"]

ident :: Parser Char -> Parser Text
ident fstChar =
  try (notReserved . T.pack =<< ((:) <$> fstChar <*> many sndChar) <?> "Identifier")
  where
    sndChar = alphaNumChar <|> char '\''

    notReserved i
      | i `elem` reservedNames = fail $ "Reserved name " <> show i
      | otherwise = pure i

varOpChars :: String
varOpChars = "!#$%&*+./<=>?@\\^|-~;"

opChars :: String
opChars = ':' : varOpChars

operator' :: Parser Char -> Parser Text
operator' fstChar =
  try (notReserved . T.pack =<< ((:) <$> fstChar <*> many opChar) <?> "Operator")
  where
    opChar = oneOf opChars

    notReserved i
      | i `elem` reservedOps = fail $ "Reserved operator " <> show i
      | otherwise = pure i

operator :: Parser Char -> Parser Text
operator = lexeme . operator'

varName' :: Parser Text
varName' = ident lowerChar

varName :: Parser Text
varName = lexeme varName'

ctorName' :: Parser Text
ctorName' = ident upperChar

ctorName :: Parser Text
ctorName = lexeme ctorName'

data Fixity = L | R

inf :: Fixity -> Parser (a -> a -> a) -> Operator Parser a
inf L = InfixL
inf R = InfixR

opE :: Parser Char -> Parser (Expr -> Expr -> Expr)
opE c = operator c <&> \o a b -> (Var (Unqual o) `App` a) `App` b

opCtor :: Parser Text
opCtor = operator (char ':')

opInfixE :: Parser (Expr -> Expr -> Expr)
opInfixE = between (char '`') (symbol "`") varName' <&> \o a b -> (Var (Unqual o) `App` a) `App` b

opInfixCtor :: Parser Text
opInfixCtor = between (char '`') (symbol "`") ctorName

varId :: Parser Text
varId = varName <|> try (between (char '(') (symbol ")") $ operator' $ oneOf varOpChars)

ctorId :: Parser Text
ctorId = ctorName <|> try (between (char '(') (symbol ")") $ operator' $ char ':')

tupLit :: ([a] -> a) -> Parser a -> Parser a
tupLit mk term = btwn "(" ")" $ mkTup <$> (try term `sepBy` symbol ",")
  where
    mkTup [a] = a
    mkTup l = mk l

recLit :: Parser a -> Parser [(Id, Maybe a)]
recLit term =
  btwn "{" "}" (field `sepBy` symbol ",") <?> "Record literal"
  where
    field = (,) <$> varId <*> optional (symbol "=" *> term)

listLit :: Parser a -> Parser [a]
listLit term = btwn "[" "]" (term `sepBy` symbol ",") <?> "List literal"

signed :: Num a => Parser (a -> a)
signed = char '-' $> negate <|> pure id

number :: Parser Number
number =
  choice
  [ NaN <$ symbol "NaN"
  , Inf <$ symbol "Infinity"
  , NegInf <$ symbol "-Infinity"
  , Num <$> (signed <*> lexeme L.scientific)
  ]
  <?> "Integer literal"

strLit :: Parser Text
strLit = lexeme (char '\"' *> manyTill L.charLiteral (char '\"') <&> T.pack) <?> "String literal"

caseBranches :: Parser [(Pat, Expr)]
caseBranches =
  optional (symbol "|")
  *> sepBy1 ((,) <$> (pat <* symbol "->" ) <*> expr) (symbol "|")

lamCase :: Parser Expr
lamCase = symbol "case" *> (LamCase <$> caseBranches)

lamVars :: Parser Expr
lamVars = Lam <$> (some patSimple <* symbol "->") <*> expr

lam :: Parser Expr
lam = symbol "\\" *> (try lamCase <|> lamVars)

binding :: Parser (Id, Expr)
binding = (,) <$> varId <*> (args <*> (symbol "=" *> expr))
  where
    args = Lam <$> some patSimple <|> pure id

bindingGroup :: Parser BindingGroup
bindingGroup = try binding `sepBy` symbol "and"

let' :: Parser Expr
let' =
  Let
  <$> (symbol "let" *> bindingGroup)
  <*> (symbol "in" *> expr)

case' :: Parser Expr
case' = Case <$> (symbol "case" *> expr <* symbol "of") <*> caseBranches

unsafeJS :: Parser Expr
unsafeJS = withWhnf <*> (btwn "[" "]" (varId `sepBy` symbol ",") <|> pure []) <*> strLit
  where
    withWhnf = UnsafeJS <$> (symbol "unsafejswhnf" $> True <|> symbol "unsafejs" $> False)

do' :: Parser Expr
do' = Do (Unqual ">>=") <$> (symbol "do" *> some (try $ stmt <* symbol "then")) <*> expr
  where
    stmt = (,) <$> optional (try $ pat <* symbol "<-") <*> expr

qualVar :: Parser Expr
qualVar = (Var .) . Qual <$> try (T.intercalate "." <$> some (ctorName' <* char '.')) <*> varId

exprSimple :: Parser Expr
exprSimple =
  choice
  [ try qualVar
  , (`CtorLit` []) <$> ctorId
  , Var . Unqual <$> varId
  , tupLit TupLit expr
  , let', lam, case', unsafeJS, do'
  , RecLit <$> recLit expr
  , ListLit <$> listLit expr
  , try $ NumLit <$> number
  , StrLit <$> strLit
  ]
  <?> "Expression"

wildcard :: Parser Pat
wildcard = symbol "_" $> Wildcard

asPat :: Parser Pat
asPat = As <$> (varId <* symbol "@") <*> patSimple

patSimple :: Parser Pat
patSimple =
  choice
  [ (`CtorPat` []) <$> ctorId
  , try asPat
  , VarPat <$> varId
  , tupLit TupPat pat
  , RecPat <$> recLit pat
  , ListPat <$> listLit pat
  , try $ NumPat <$> number
  , StrPat <$> strLit
  , wildcard
  ]
  <?> "Pattern"

patCtorApp :: Parser Pat
patCtorApp = try (CtorPat <$> ctorId <*> some patSimple) <|> patSimple

patOps :: [[Operator Parser Pat]]
patOps =
  [ [ inf R $ ctorPat <$> opCtor ]
  , [ inf R $ ctorPat <$> opInfixCtor ]
  ]
  where
    ctorPat o a b = CtorPat o [a, b]

pat :: Parser Pat
pat = makeExprParser patCtorApp patOps

exprField :: Parser Expr
exprField = foldl' (&) <$> exprSimple <*> many (try $ char '.' *> field)
  where
    field = flip CtorField <$> lexeme L.decimal <|> flip RecField <$> varId

exprApp :: Parser Expr
exprApp =
  try (CtorLit <$> ctorId <*> some exprField)
  <|> foldl1' App <$> some exprField

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
  , [ inf R $ ctorLit <$> opCtor ]
  , [ inf L opInfixE ]
  , [ inf R $ ctorLit <$> opInfixCtor ]
  ]
  where
    ctorLit o a b = CtorLit o [a, b]

expr :: Parser Expr
expr = makeExprParser exprApp exprOps

modName :: Parser Id
modName = lexeme $ T.intercalate "." <$> ctorName' `sepBy1` char '.'

import' :: Parser Id
import' = symbol "import" *> modName

module' :: Parser Module
module' =
  Module
  <$> (symbol "module" *> modName)
  <*> (strLit <|> pure "")
  <*> many import'
  <*> bindingGroup

withShebang :: Parser a -> Parser a
withShebang p = optional shebang *> optional sc *> p <* eof

parseModule :: FilePath -> Text -> Either Text Module
parseModule path = first (T.pack . errorBundlePretty) . runParser (withShebang module') path

parse :: [(FilePath, Text)] -> Either Text Program
parse ms = Program "" [] <$> traverse (uncurry parseModule) ms
