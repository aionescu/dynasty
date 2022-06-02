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
infixr 9 ...
