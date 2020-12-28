module Language.Dynasty.Runtime.Prelude where

import Data.Function(fix)
import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M

import Language.Dynasty.Frontend.Syntax
import Language.Dynasty.Runtime.Val

type Env = Map Ident Val

hang :: Bool
hang = hang

mergeRecords :: Val -> Val -> Val
mergeRecords (Rec as) (Rec bs) = Rec $ M.union bs as
mergeRecords _ _ = exn "Need records for merge"

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
  ]
