module Kern.Core where

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