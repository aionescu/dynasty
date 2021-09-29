module Language.Dynasty.Runtime.FFI where

import Data.Proxy(Proxy(..))
import Data.Text(Text)
import Data.Typeable(Typeable, typeRep)
import Data.Vector qualified as V

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
