module Utils where

import Data.Text(Text)
import qualified Data.Text as T
import Text.Read(readMaybe)

showT :: Show a => a -> Text
showT = T.pack . show

readT :: Read a => Text -> Maybe a
readT = readMaybe . T.unpack
