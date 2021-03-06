module Language.Dynasty.Runtime.Val where

import Data.Char(isLetter)
import Data.Foldable(toList)
import Data.List(intercalate)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Data.Proxy(Proxy(..))
import Data.Typeable(Typeable, typeRep)
import Text.Parsec
import Text.Read(readMaybe)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Frontend.Parser hiding (parse)

data Val
  = Num Integer
  | Char Char
  | Ctor Ident [Val]
  | Rec (Map String Val)
  | Fn (Val -> Val)
  | IO (IO Val)
  deriving stock Typeable

exn :: String -> Val
exn s = Ctor "Exception" [toVal s]

class ToVal a where
  toVal :: a -> Val

class OfVal a where
  ofVal :: Val -> Maybe a

instance ToVal Val where
  toVal = id

instance OfVal Val where
  ofVal = Just

instance ToVal Integer where
  toVal = Num

instance OfVal Integer where
  ofVal (Num i) = Just i
  ofVal _ = Nothing

instance ToVal Char where
  toVal = Char

instance OfVal Char where
  ofVal (Char c) = Just c
  ofVal _ = Nothing

instance ToVal Bool where
  toVal b = Ctor (show b) []

instance OfVal Bool where
  ofVal (Ctor i []) = readMaybe i
  ofVal _ = Nothing

instance ToVal a => ToVal (IO a) where
  toVal = IO . (toVal <$>)

instance OfVal (IO Val) where
  ofVal (IO io) = Just io
  ofVal _ = Nothing

instance ToVal () where
  toVal _ = Ctor "Tuple" []

instance OfVal () where
  ofVal (Ctor "Tuple" []) = Just ()
  ofVal _ = Nothing

-- TODO: GHC.Generic-based default implementations
instance ToVal a => ToVal [a] where
  toVal [] = Ctor "Nil" []
  toVal (a : as) = Ctor "::" [toVal a, toVal as]

instance OfVal a => OfVal [a] where
  ofVal (Ctor "Nil" []) = Just []
  ofVal (Ctor "::" [a, as]) = (:) <$> ofVal a <*> ofVal as
  ofVal _ = Nothing

instance ToVal a => ToVal (Maybe a) where
  toVal Nothing = Ctor "Nothing" []
  toVal (Just a) = Ctor "Just" [toVal a]

instance OfVal a => OfVal (Maybe a) where
  ofVal (Ctor "Nothing" []) = Just Nothing
  ofVal (Ctor "Just" [a]) = Just <$> ofVal a
  ofVal _ = Nothing

instance (Typeable a, OfVal a, ToVal b) => ToVal (a -> b) where
  toVal f = Fn \v ->
    case ofVal v of
      Nothing -> exn $ "Expected something that looks like '" ++ show (typeRep $ Proxy @a) ++ "', but found: " ++ show v ++ "."
      Just a -> toVal $ f a

instance OfVal (Val -> Val) where
  ofVal (Fn f) = Just f
  ofVal _ = Nothing

instance OfVal (Val -> IO Val) where
  ofVal (Fn f) = Just \a ->
    case f a of
      IO v -> v
      v -> pure $ exn $ "Expected IO value, found pure value: " ++ show v ++ "."

  ofVal _ = Nothing

showIdent :: String -> String
showIdent i | isLetter $ head i = i
showIdent i = "(" ++ i ++ ")"

showRecord :: Map String Val -> String
showRecord r
  | M.null r = "{ }"
  | otherwise = "{ " ++ intercalate ", " (uncurry showField <$> M.toList r) ++ " }"
  where
    showField i v = showIdent i ++ " = " ++ showVal False v

showList' :: Show a => [a] -> String
showList' l = "[" ++ intercalate ", " (show <$> l) ++ "]"

showCtor :: Bool -> Ident -> [Val] -> String
showCtor _ "Tuple" [] = "()"
showCtor _ "Tuple" [a] = "(" ++ showVal False a ++ ",)"
showCtor _ "Tuple" as = "(" ++ intercalate ", " (showVal False <$> as) ++ ")"

showCtor _ "Nil" [] = "[]"
showCtor _ "::" as
  | Just (s :: String) <- ofVal (Ctor "::" as) = show s
  | Just (l :: [Val]) <- ofVal (Ctor "::" as) = showList' l

showCtor _ i [] = showIdent i

showCtor False i [a, b]
  | not $ isLetter $ head i = showVal True a ++ " " ++ i ++ " " ++ showVal True b
showCtor True i [a, b]
  | not $ isLetter $ head i = "(" ++ showVal True a ++ " " ++ i ++ " " ++ showVal True b ++ ")"

showCtor False i as = showIdent i ++ " " ++ unwords (showVal True <$> as)
showCtor True i as = "(" ++ showIdent i ++ " " ++ unwords (showVal True <$> as) ++ ")"

showVal :: Bool -> Val -> String
showVal _ (Num i) = show i
showVal _ (Char c) = show c
showVal _ (Rec m) = showRecord m
showVal needParens (Ctor i as) = showCtor needParens i as
showVal _ Fn{} = "<λ>"
showVal _ IO{} = "<🗲>"

instance Show Val where
  show = showVal False

instance Eq Val where
  Num a == Num b = a == b
  Char a == Char b = a == b
  Ctor a as == Ctor b bs = a == b && as == bs
  Rec as == Rec bs = as == bs
  _ == _ = False

numVal :: Parser Val
numVal = Num <$> intRaw

charVal :: Parser Val
charVal = Char <$> charRaw

strVal :: Parser Val
strVal = toVal <$> strRaw

tupVal :: Parser Val
tupVal = tuple (Ctor "Tuple") val

listVal :: Parser Val
listVal = list toVal val

fieldVal :: Parser (Ident, Val)
fieldVal = (,) <$> (varIdent <* equals) <*> val

recVal :: Parser Val
recVal = record fieldVal \m b ->
  if b
  then fail "Record wildcards can only appear in patterns"
  else pure $ Rec m

ctorValSimple :: Parser Val
ctorValSimple = (`Ctor` []) <$> ctorIdent

valSimple :: Parser Val
valSimple = choice (try <$> [recVal, listVal, tupVal, strVal, charVal, numVal, ctorValSimple]) <* ws

valCtorApp :: Parser Val
valCtorApp = try (Ctor <$> (ctorIdent <* ws) <*> many (valSimple <* ws)) <|> valSimple

appCtorVal :: Ident -> Val -> Val -> Val
appCtorVal c a b = Ctor c [a, b]

valCtorOps :: Parser Val
valCtorOps = chainl1 valCtorApp $ try $ appCtorVal <$> (ctorInfix <* ws)

val :: Parser Val
val = valCtorOps

withRest :: Parser a -> Parser (a, String)
withRest p = (,) <$> p <*> getInput

instance Read Val where
  readsPrec _ = toList . parse (withRest val) ""
