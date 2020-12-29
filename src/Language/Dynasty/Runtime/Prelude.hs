module Language.Dynasty.Runtime.Prelude where

import Data.Function(fix)
import Data.Map.Lazy(Map)
import qualified Data.Map.Lazy as M
import Debug.Trace(trace)

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val

type Env = Map Ident Val

hang :: Bool
hang = hang

mergeRecords :: Val -> Val -> Val
mergeRecords (Rec as) (Rec bs) = Rec $ M.union bs as
mergeRecords _ _ = exn "Need records for merge"

typeOf :: Val -> Val
typeOf (Num _) = Ctor "Num" []
typeOf (Char _) = Ctor "Char" []
typeOf (Ctor i _) = Ctor "Ctor" [toVal i]
typeOf (Rec m) = Ctor "Rec" [toVal $ M.keys m]
typeOf (Fn _) = Ctor "Fn" []
typeOf (IO _) = Ctor "IO" []

prelude :: Env
prelude =
  M.fromList
  [ ("+", toVal $ (+) @Integer)
  , ("-", toVal $ (-) @Integer)
  , ("*", toVal $ (*) @Integer)
  , ("/", toVal $ quot @Integer)
  , ("%", toVal $ rem @Integer)
  , ("not", toVal not)
  , ("&&", toVal (&&))
  , ("||", toVal (||))
  , ("pure", toVal $ pure @IO @Val)
  , ("*>", toVal $ (*>) @IO @Val @Val)
  , (">>=", toVal $ (>>=) @IO @Val @Val)
  , ("getLine", toVal getLine)
  , ("print", toVal $ print @Val)
  , ("++", toVal $ (++) @Val)
  , ("hang", toVal hang)
  , ("show", toVal $ show @Val)
  , ("fix", toVal $ fix @Val) -- Free recursion
  , ("==", toVal $ (==) @Val)
  , (".", toVal $ (.) @Val @Val @Val)
  , ("&", toVal mergeRecords)
  , ("trace", toVal $ trace @Val)
  , ("typeOf", toVal typeOf)
  ]
