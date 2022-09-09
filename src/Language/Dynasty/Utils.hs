module Language.Dynasty.Utils where

import Control.Monad.Except(MonadError(throwError))
import Data.Foldable(foldlM)
import Data.Set qualified as S
import Data.Text(Text)
import Data.Text qualified as T

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)
infixl 8 ...

err :: MonadError e m => e -> m a
err = throwError

showT :: Show a => a -> Text
showT = T.pack . show

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go _ [] = []
    go i (a : as) = f i a : go (succ i) as

foldMapM :: (Foldable t, Monad m, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldlM (\b a -> (b <>) <$> f a) mempty

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = go S.empty
  where
    go _ [] = Nothing
    go s (a : as)
      | S.member a s = Just a
      | otherwise = go (S.insert a s) as
