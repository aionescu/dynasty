module Utils where

import Data.Text(Text)
import Data.Text qualified as T

showT :: Show a => a -> Text
showT = T.pack . show

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go _ [] = []
    go i (a : as) = f i a : go (succ i) as
