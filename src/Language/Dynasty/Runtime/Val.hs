{-# LANGUAGE StrictData #-}

module Language.Dynasty.Runtime.Val where

import Data.Char(isLetter)
import Data.Map.Lazy(Map)
import Data.Map.Lazy qualified as M
import Data.Text(Text)
import Data.Text qualified as T
import Data.Typeable(Typeable)
import Data.Vector(Vector)
import Data.Vector qualified as V

import Language.Dynasty.Frontend.Syntax
import Utils

data Val
  = Num Integer
  | Char Char
  | Str Text
  | Ctor Ident (Vector Val)
  | Rec (Map Ident Val)
  | Fn (Val -> Val)
  | IO (IO Val)
  deriving stock Typeable

exn :: Text -> Val
exn s = Ctor "Exception" [Str s]

showIdent :: Ident -> Text
showIdent i | isLetter $ T.head i = i
showIdent i = "(" <> i <> ")"

showRecord :: Map Ident Val -> Text
showRecord r
  | M.null r = "{ }"
  | otherwise = "{ " <> T.intercalate ", " (uncurry showField <$> M.toList r) <> " }"
  where
    showField i v = showIdent i <> " = " <> showVal False v

showList' :: Show a => [a] -> Text
showList' l = "[" <> T.intercalate ", " (showT <$> l) <> "]"

unwrapList :: Val -> Maybe [Val]
unwrapList (Ctor "Nil" []) = Just []
unwrapList (Ctor "::" [v, vs]) = (v :) <$> unwrapList vs
unwrapList _ = Nothing

showCtor :: Bool -> Ident -> Vector Val -> Text
showCtor _ "Tuple" [] = "()"
showCtor _ "Tuple" [a] = "(" <> showVal False a <> ",)"
showCtor _ "Tuple" as = "(" <> T.intercalate ", " (showVal False <$> V.toList as) <> ")"

showCtor _ "Nil" [] = "[]"
showCtor _ "::" [v, vs]
  | Just vs' <- unwrapList vs = showList' (v : vs')

showCtor _ i [] = showIdent i

showCtor False i [a, b]
  | not $ isLetter $ T.head i = showVal True a <> " " <> i <> " " <> showVal True b
showCtor True i [a, b]
  | not $ isLetter $ T.head i = "(" <> showVal True a <> " " <> i <> " " <> showVal True b <> ")"

showCtor False i as = showIdent i <> " " <> T.unwords (showVal True <$> V.toList as)
showCtor True i as = "(" <> showIdent i <> " " <> T.unwords (showVal True <$> V.toList as) <> ")"

showVal :: Bool -> Val -> Text
showVal _ (Num i) = showT i
showVal _ (Char c) = showT c
showVal _ (Str s) = showT s
showVal _ (Rec m) = showRecord m
showVal needParens (Ctor i as) = showCtor needParens i as
showVal _ Fn{} = "<λ>"
showVal _ IO{} = "<🗲>"

instance Show Val where
  show = T.unpack . showVal False

instance Eq Val where
  Num a == Num b = a == b
  Char a == Char b = a == b
  Ctor a as == Ctor b bs = a == b && as == bs
  Rec as == Rec bs = as == bs
  _ == _ = False
