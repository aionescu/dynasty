module Utils where

import Data.Text(Text)
import Data.Text qualified as T
import Text.Read(readMaybe)

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> Maybe a
readT = readMaybe . T.unpack

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(...) = (.) . (.)
infixr 8 ...

imap :: (Int -> a -> b) -> [a] -> [b]
imap f = go 0
  where
    go _ [] = []
    go i (a : as) = f i a : go (succ i) as
