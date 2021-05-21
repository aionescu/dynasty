module Language.Dynasty.Runtime.Prelude where

import Control.Category((>>>))
import Data.IORef(IORef, newIORef, writeIORef, readIORef)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Debug.Trace(trace)
import System.IO.Unsafe(unsafePerformIO)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val

type Env = Map Ident Val

mergeRecords :: Val -> Val -> Val
mergeRecords (Rec as) (Rec bs) = Rec $ M.union bs as
mergeRecords _ _ = exn "Need records for merge"

typeOf :: Val -> Val
typeOf (Num _) = Ctor "Num" []
typeOf (Char _) = Ctor "Char" []
typeOf (Ctor i as) = Ctor "Ctor" [toVal i, toVal $ toInteger $ length as]
typeOf (Rec m) = Ctor "Rec" [toVal $ M.keys m]
typeOf (Fn _) = Ctor "Fn" []
typeOf (IO _) = Ctor "IO" []

-- Ugly, but works for now
{-# NOINLINE argsRef #-}
argsRef :: IORef [String]
argsRef = unsafePerformIO $ newIORef []

args :: IO Val
args = toVal . (toVal <$>) <$> readIORef argsRef

setArgs :: [String] -> IO ()
setArgs = writeIORef argsRef

prelude :: Env
prelude =
  M.fromList
  [ ("+", toVal $ (+) @Integer)
  , ("-", toVal $ (-) @Integer)
  , ("*", toVal $ (*) @Integer)
  , ("/", toVal $ quot @Integer)
  , ("%", toVal $ rem @Integer)
  , ("<$>", toVal $ (<$>) @IO @Val @Val)
  , ("pure", toVal $ pure @IO @Val)
  , ("*>", toVal $ (*>) @IO @Val @Val)
  , (">>=", toVal $ (>>=) @IO @Val @Val)
  , ("getLine", toVal getLine)
  , ("print", toVal $ print @Val)
  , ("putStrLn", toVal putStrLn)
  , ("show", toVal $ show @Val)
  , ("read", toVal $ read @Val)
  , ("==", toVal $ (==) @Val)
  , (".", toVal $ (.) @Val @Val @Val)
  , ("&", toVal mergeRecords)
  , ("trace", toVal $ trace @Val)
  , ("typeOf", toVal typeOf)
  , ("<", toVal $ (<) @Integer)
  , ("readFile", toVal readFile)
  , ("getChar", toVal getChar)
  , ("putChar", toVal putChar)
  , ("getArgs", toVal args)
  , ("charToNum", toVal $ toInteger . fromEnum @Char)
  , ("numToChar", toVal $ toEnum @Char . fromInteger)
  , (";", toVal $ (>>>) @(->) @Val @Val @Val)
  ]
