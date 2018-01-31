{-# LANGUAGE OverloadedStrings #-}

module Kern.Grammar where

import Data.Text
import Data.Yaml

data StorageClass
  = Auto
  | Register
  | Static
  | Extern
  | Typedef
  deriving (Eq, Show, Ord)

data TypeQualifier
  = ConstQualifier
  | VolatileQualifier
  deriving (Eq, Show, Ord)

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

data TypeSpecifier
  = SpecVoid
  | SpecChar
  | SpecShort
  | SpecInt
  | SpecLong
  | SpecFloat
  | SpecDouble
  | SpecSigned
  | SpecUnsigned
  deriving (Eq, Show, Ord)

data DeclarationSpecifier
  = StorageClass StorageClass
  | TypeSpecifier TypeSpecifier
  | TypeQualifier TypeQualifier
  deriving (Eq, Show)

data Linkage = External | Internal deriving (Eq, Show)
data Constness = Constant | Mutable deriving (Eq, Show)
data Volatility = Volatile | NonVolatile deriving (Eq, Show)
data Indirection = Direct | Indirect Pointer deriving (Eq, Show)

instance ToJSON Linkage where
  toJSON External = String "extern"
  toJSON Internal = String "intern"

instance ToJSON Constness where
  toJSON Constant = String "const"
  toJSON Mutable = String "mut"

instance ToJSON Volatility where
  toJSON Volatile = String "volatile"
  toJSON NonVolatile = String "nonvolatile"

instance ToJSON Indirection where
  toJSON Direct = String "direct"
  toJSON (Indirect p) = object [("indirect", toJSON p)]

data ExternalType =
  ExternalType DataType
               Linkage
               Constness
               Volatility
               Indirection
  deriving (Eq, Show)

instance ToJSON ExternalType where
  toJSON (ExternalType dt l c v i) =
    object [ ("linkage", toJSON l)
           , ("constness", toJSON c)
           , ("volatility", toJSON v)
           , ("indirection", toJSON i)
           , ("datatype", toJSON dt)
           ]

data InternalType =
  InternalType DataType
               Constness
               Volatility
               Indirection
  deriving (Eq, Show)

instance ToJSON InternalType where
  toJSON (InternalType dt c v i) =
    object [ ("constness", toJSON c)
           , ("volatility", toJSON v)
           , ("indirection", toJSON i)
           , ("datatype", toJSON dt)
           ]

data ParameterType =
  ParameterType DataType
                Constness
                Volatility
                Indirection
  deriving (Eq, Show)

instance ToJSON ParameterType where
  toJSON (ParameterType dt c v i) =
    object [ ("constness", toJSON c)
           , ("volatility", toJSON v)
           , ("indirection", toJSON i)
           , ("datatype", toJSON dt)
           ]

newtype Pointer = Pointer (Maybe Pointer)
               deriving (Eq, Show)

instance ToJSON Pointer where
  toJSON (Pointer p) = String "*"

newtype Identifier = Ident Text deriving (Eq, Show)

instance ToJSON Identifier where
  toJSON (Ident t) = String t

type Params = [Parameter]
data Parameter = Parameter ParameterType Identifier deriving (Eq, Show)

instance ToJSON Parameter where
  toJSON (Parameter ty ident) =
    object [ ("type", toJSON ty)
           , ("id", toJSON ident)]

data Initialization = NotInitialized | Initializer deriving (Eq, Show)

data LocalVariable = LocalVariable InternalType Identifier Initialization
                     deriving (Eq, Show)

instance ToJSON LocalVariable where
  toJSON (LocalVariable ty ident ini) =
    object [ ("type", toJSON ty)
           , ("id", toJSON ident) ]

data FunctionPrototype = Prototype ExternalType Identifier Params
                         deriving (Eq, Show)

instance ToJSON FunctionPrototype where
  toJSON (Prototype ty ident ps) =
    object [ ("return-type", toJSON ty)
           , ("id", toJSON ident)]

data Statement = Jump Jump deriving (Eq, Show)

data Body = Body [LocalVariable] [Statement]
            deriving (Eq, Show)

instance ToJSON Statement where
  toJSON (Jump j) = toJSON j

instance ToJSON Body where
  toJSON (Body vars stms) =
    object [ ("local-vars", array $ fmap toJSON vars)
           , ("statements", array $ fmap toJSON stms)
           ]

data FunctionDefinition =
  Function ExternalType Identifier Params Body
  deriving (Eq, Show)

instance ToJSON FunctionDefinition where
  toJSON (Function ty ident ps body) =
    object [ ("return-type", toJSON ty)
           , ("id", toJSON ident)
           , ("parameters", array $ fmap toJSON ps)
           , ("body", toJSON body)]

data Jump
  = Goto Identifier
  | Continue
  | Break
  | ReturnVoid
  deriving (Eq, Show)

instance ToJSON Jump where
  toJSON (Goto ident) = object [ ("goto", toJSON ident) ]
  toJSON Break        = String "break"
  toJSON Continue     = String "continue"
  toJSON ReturnVoid   = String "returnvoid"

newtype TranslationUnit =
  TranslationUnit [ExternalDeclaration]
  deriving (Eq, Show)

instance ToJSON TranslationUnit where
  toJSON (TranslationUnit decls) =
    object [ ("translation-unit", array $ fmap toJSON decls) ]

data ExternalDeclaration
  = FunctionDefinition FunctionDefinition
  deriving (Eq, Show)

instance ToJSON ExternalDeclaration where
  toJSON (FunctionDefinition fd) =
    object [ ("function-definition", toJSON fd) ]
