module Language.Dynasty.Runtime.Prelude where

import Control.Category((>>>))
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import Debug.Trace(trace)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val
import Language.Dynasty.Runtime.FFI
import Utils

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
