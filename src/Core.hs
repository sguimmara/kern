{-# LANGUAGE OverloadedStrings #-}
module Core
    ( TypeSpecifier (..)
    , TypeQualifier (..)
    ) where

import Data.Yaml

data TypeSpecifier =
  VoidT
  | IntT
  | ShortT
  | FloatT
  | DoubleT
  | LongT
  | UnsignedT
  | SignedT
    deriving (Eq, Show)

data TypeQualifier =
  ConstQ
  | VolatileQ
  deriving (Eq, Show)

instance ToJSON TypeSpecifier where
  toJSON VoidT     = String "void"
  toJSON IntT      = String "int"
  toJSON ShortT    = String "short"
  toJSON LongT     = String "long"
  toJSON FloatT    = String "float"
  toJSON DoubleT   = String "double"
  toJSON SignedT   = String "signed"
  toJSON UnsignedT = String "unsigned"

instance ToJSON TypeQualifier where
  toJSON ConstQ    = String "const"
  toJSON VolatileQ = String "volatile"
