{-# LANGUAGE OverloadedStrings #-}

module Kern.Bytecode where

import           Kern.Core
import           Kern.AST

import           Data.Word
import           Data.List
import           Data.Int
import           Data.Yaml
import           Data.Vector      (Vector)
import qualified Data.Vector as V
import           Data.Text        (Text, pack, toLower)

data Local = Local DataType deriving (Eq, Show)

instance ToJSON Local where
  toJSON (Local dt) = object [ ("type", toJSON dt) ]

data Routine =
  Routine { routName :: Text
          , locals :: (Vector Local)
          , instrs :: (Vector Instr)
          } deriving (Eq, Show)

instance ToJSON Routine where
  toJSON r =
    object
      [ ("locals",       (array . V.toList . (V.map toJSON)) (locals r))
      , ("instructions", (array . V.toList . (V.map toJSON)) (instrs r))
      ]

data Instr
  = Add           -- Replace the 2 last stack values with their sum
  | Sub           -- Replace the 2 last stack values with their difference
  | Ldc64 Int64   -- push signed 64bit word onto the stack
  | Ldc32 Int32   -- push signed 64bit word onto the stack
  | Ret64         -- exit from current routine, returning no value
  | Ret           -- exit from current routine, returning no value
  | Stloc Int     -- Pops top stack value into local variable N
  | Ldloc Int     -- Push local variable N onto the stack
  deriving (Eq, Show, Ord)

instance ToJSON Instr where
  toJSON i =
    case i of
      Stloc n   -> String $ pack ("stloc " ++ show n)
      Ldloc n   -> String $ pack ("ldloc " ++ show n)
      Ldc64 i64 -> String $ pack ("ldc64 " ++ show i64)
      Ldc32 i32 -> String $ pack ("ldc32 " ++ show i32)
      x         -> String $ toLower $ pack (show x)

data Context
  = FuncContext FunctionDefinition
  deriving (Eq, Show)

getLocIdx :: Identifier -> Context -> Int
getLocIdx i0 (FuncContext fn) =
  let locs = (bodyLocals . funcBody) fn
      idx = findIndex (\(LocalVariable _ i1 _) -> i0 == i1) locs in
  case idx of
    Just i  -> i
    Nothing -> error "could not locate local variable"

class Emittable a where
  emit :: Context -> a -> Vector Instr

instance Emittable Expr where
  emit ctx (AddExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Add
  emit ctx (SubExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Sub
  emit ctx (PrimC (Int64Lit n)) = V.fromList [Ldc64 n]
  emit ctx (PrimC (Int32Lit n)) = V.fromList [Ldc32 n]
  emit ctx (PrimI ident)        = V.fromList [Ldloc n]
                                  where n = getLocIdx ident ctx

