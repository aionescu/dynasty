module Language.Dynasty.Runtime.Prelude where

import Control.Category((>>>))
import Data.Map.Lazy(Map)
import Data.Map.Lazy qualified as M
import Data.Proxy(Proxy(..))
import Data.Text(Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Data.Typeable(Typeable, typeRep)
import Data.Vector qualified as V
import Debug.Trace(trace)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val
import Utils

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

instance ToVal Text where
  toVal = Str

instance OfVal Text where
  ofVal (Str s) = Just s
  ofVal _ = Nothing

instance ToVal Bool where
  toVal b = Ctor (showT b) V.empty

instance OfVal Bool where
  ofVal (Ctor i []) = readT i
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
      Nothing -> exn $ "Expected something that looks like '" <> showT (typeRep $ Proxy @a) <> "', but found: " <> showT v <> "."
      Just a -> toVal $ f a

instance OfVal (Val -> Val) where
  ofVal (Fn f) = Just f
  ofVal _ = Nothing

instance OfVal (Val -> IO Val) where
  ofVal (Fn f) = Just \a ->
    case f a of
      IO v -> v
      v -> pure $ exn $ "Expected IO value, found pure value: " <> showT v <> "."

  ofVal _ = Nothing

type Env = Map Ident Val

mergeRecords :: Val -> Val -> Val
mergeRecords (Rec as) (Rec bs) = Rec $ M.union bs as
mergeRecords _ _ = exn "Need records for merge"

typeOf :: Val -> Val
typeOf (Num _) = Ctor "Num" []
typeOf (Char _) = Ctor "Char" []
typeOf (Str _) = Ctor "Str" []
typeOf (Ctor i as) = Ctor "Ctor" [toVal i, toVal $ toInteger $ length as]
typeOf (Rec m) = Ctor "Rec" [toVal $ M.keys m]
typeOf (Fn _) = Ctor "Fn" []
typeOf (IO _) = Ctor "IO" []

prelude :: [Text] -> Env
prelude args =
  M.fromList
  [ ("+", toVal $ (+) @Integer)
  , ("-", toVal $ (-) @Integer)
  , ("*", toVal $ (*) @Integer)
  , ("/", toVal $ quot @Integer)
  , ("%", toVal $ rem @Integer)
  , ("^", toVal $ (^) @Integer @Integer)
  , ("<$>", toVal $ (<$>) @IO @Val @Val)
  , ("pure", toVal $ pure @IO @Val)
  , ("*>", toVal $ (*>) @IO @Val @Val)
  , (">>=", toVal $ (>>=) @IO @Val @Val)
  , ("getLine", toVal T.IO.getLine)
  , ("print", toVal $ print @Val)
  , ("putStrLn", toVal T.IO.putStrLn)
  , ("show", toVal $ showT @Val)
  , ("read", toVal $ readT @Val)
  , ("==", toVal $ (==) @Val)
  , (".", toVal $ (.) @Val @Val @Val)
  , ("&", toVal mergeRecords)
  , ("trace", toVal $ trace @Val . T.unpack)
  , ("typeOf", toVal typeOf)
  , ("<", toVal $ (<) @Integer)
  , ("readFile", toVal $ T.IO.readFile . T.unpack)
  , ("getChar", toVal getChar)
  , ("putChar", toVal putChar)
  , ("getArgs", toVal $ pure @IO args)
  , ("charToNum", toVal $ toInteger . fromEnum @Char)
  , ("numToChar", toVal $ toEnum @Char . fromInteger)
  , (";", toVal $ (>>>) @(->) @Val @Val @Val)
  , ("explode", toVal T.unpack)
  , ("implode", toVal T.pack)
  ]
