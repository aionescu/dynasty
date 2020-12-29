module Language.Dynasty.Runtime.Val where

import Control.Exception(catch)
import Control.Exception.Base(SomeException)
import Data.Char(isLetter)
import Data.List(intercalate)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import System.IO.Unsafe(unsafePerformIO)
import Text.Read(readMaybe)

import Language.Dynasty.Frontend.Syntax

data Val
  = Num Integer
  | Char Char
  | Ctor Ident [Val]
  | Rec (Map String Val)
  | Fn (Val -> Val)
  | IO (IO Val)

exn :: String -> Val
exn s = Ctor "Exception" [toVal s]

catchExn :: Val -> Val
catchExn v = unsafePerformIO $ pure v `catch` \(e :: SomeException) -> pure $ exn $ show e

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
  ofVal = \case
    Num i -> Just i
    _ -> Nothing

instance ToVal Char where
  toVal = Char

instance OfVal Char where
  ofVal = \case
    Char c -> Just c
    _ -> Nothing

instance ToVal Bool where
  toVal b = Ctor (show b) []

instance OfVal Bool where
  ofVal = \case
    Ctor i [] -> readMaybe i
    _ -> Nothing

instance ToVal a => ToVal (IO a) where
  toVal = IO . (toVal <$>)

instance OfVal (IO Val) where
  ofVal = \case
    IO v -> Just v
    _ -> Nothing

instance ToVal () where
  toVal _ = Ctor "Tuple" []

instance OfVal () where
  ofVal = \case
    Ctor "Tuple" [] -> Just ()
    _ -> Nothing

-- TODO: GHC.Generic-based default implementations
instance ToVal a => ToVal [a] where
  toVal [] = Ctor "Nil" []
  toVal (a : as) = Ctor "::" [toVal a, toVal as]

instance OfVal a => OfVal [a] where
  ofVal = \case
    Ctor "Nil" [] -> Just []
    Ctor "::" [a, as] -> (:) <$> ofVal a <*> ofVal as
    _ -> Nothing

instance ToVal a => ToVal (Maybe a) where
  toVal Nothing = Ctor "Nothing" []
  toVal (Just a) = Ctor "Just" [toVal a]

instance OfVal a => OfVal (Maybe a) where
  ofVal = \case
    Ctor "Nothing" [] -> Just Nothing
    Ctor "Just" [a] -> Just <$> ofVal a
    _ -> Nothing

instance (OfVal a, ToVal b) => ToVal (a -> b) where
  toVal f = Fn $ maybe (exn "Input failed in ofVal @(a -> b).") (toVal . f) . ofVal

instance OfVal (Val -> Val) where
  ofVal = \case
    Fn f -> Just f
    _ -> Nothing

instance OfVal (Val -> IO Val) where
  ofVal = \case
    Fn f ->
      Just \a -> case f a of
        IO v -> v
        _ -> pure $ exn "Expected IO value, found pure value"
    _ -> Nothing

showIdent :: String -> String
showIdent i | isLetter $ head i = i
showIdent i = "(" ++ i ++ ")"

showRecord :: Map String Val -> String
showRecord r
  | M.null r = "{ }"
  | otherwise = "{ " ++ intercalate ", " (uncurry showField <$> M.toList r) ++ " }"
  where
    showField i v = showIdent i ++ " = " ++ showVal False v

showCtor :: Bool -> Ident -> [Val] -> String
showCtor _ "Tuple" [] = "()"
showCtor _ "Tuple" [a] = "(" ++ showVal False a ++ ",)"
showCtor _ "Tuple" as = "(" ++ intercalate ", " (showVal False <$> as) ++ ")"

showCtor _ "Nil" [] = "[]"
showCtor _ "::" as
  | Just (s :: String) <- ofVal (Ctor "::" as) = show s
  | Just (l :: [Val]) <- ofVal (Ctor "::" as) = show l

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
showVal parens (Ctor i as) = showCtor parens i as
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
