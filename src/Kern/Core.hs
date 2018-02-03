{-# LANGUAGE OverloadedStrings #-}
module Kern.Core where

import Data.Int
import Data.Yaml

data DataType
  = TyVoid
  | TyByte
  | TyInt16
  | TyInt32
  | TyInt64
  | TyUint16
  | TyUint32
  | TyUint64
  | TyFloat32
  | TyFloat64
  deriving (Eq, Show)

instance ToJSON DataType where
  toJSON TyVoid = String "void"
  toJSON TyByte = String "byte"
  toJSON TyInt16 = String "i16"
  toJSON TyInt32 = String "i32"
  toJSON TyInt64 = String "i64"
  toJSON TyUint16 = String "u16"
  toJSON TyUint32 = String "u32"
  toJSON TyUint64 = String "u64"
  toJSON TyFloat32 = String "f32"
  toJSON TyFloat64 = String "f64"

data Literal
  = Int32Lit Int32
  | Int64Lit Int64
  | CharLit Char
  | FloatLit Float
  deriving (Eq, Show)

instance ToJSON Literal where
  toJSON (Int32Lit n) = object [ ("int32-literal", toJSON n) ]
  toJSON (Int64Lit n) = object [ ("int64-literal", toJSON n) ]
  toJSON (CharLit c) = object [ ("char-literal", toJSON c) ]
  toJSON (FloatLit f) = object [ ("float-literal", toJSON f) ]