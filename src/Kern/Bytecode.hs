{-# LANGUAGE OverloadedStrings #-}

module Kern.Bytecode where

import Kern.Core

import           Data.Word
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
  | Ldc64 Int64   -- push signed 64bit word onto the stack
  | Ret64         -- exit from current routine, returning no value
  | Ret           -- exit from current routine, returning no value
  | Stloc Int     -- Pops top stack value into local variable N
  deriving (Eq, Show, Ord)

instance ToJSON Instr where
  toJSON i =
    case i of
      Stloc n   -> String $ pack ("stloc " ++ show n)
      Ldc64 i64 -> String $ pack ("ldc64 " ++ show i64)
      x         -> String $ toLower $ pack (show x)

