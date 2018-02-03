{-# LANGUAGE OverloadedStrings #-}

module Kern.AST where

import Data.Text as T
import Data.Yaml

import Kern.Core

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

data Initializer
  = InitExpr Expr
  deriving (Eq, Show)

instance ToJSON Initializer where
  toJSON (InitExpr e) = object [ ("assignment-expr", toJSON e) ]

data LocalVariable =
  LocalVariable InternalType Identifier (Maybe Initializer)
  deriving (Eq, Show)

instance ToJSON LocalVariable where
  toJSON (LocalVariable ty ident ini) =
    object [ ("type", toJSON ty)
           , ("id", toJSON ident)
           , ("initializer", toJSON ini)
           ]

data FunctionPrototype = Prototype ExternalType Identifier Params
                         deriving (Eq, Show)

instance ToJSON FunctionPrototype where
  toJSON (Prototype ty ident ps) =
    object [ ("return-type", toJSON ty)
           , ("id", toJSON ident)]

data Statement
  = Jump Jump
  | ExprStmt (Maybe Expr)
  deriving (Eq, Show)

data Body = Body [LocalVariable] [Statement]
            deriving (Eq, Show)

instance ToJSON Statement where
  toJSON (Jump j) = object [ ("jump-statement", toJSON j) ]
  toJSON (ExprStmt Nothing) = String "empty-expr-statement"
  toJSON (ExprStmt (Just e)) = object [ ("expr-statement", toJSON e) ]

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

data Expr
  = Or Expr Expr
  | And Expr Expr
  | BitwiseOr Expr Expr
  | BitwiseAnd Expr Expr
  | Xor Expr Expr
  | CondExpr Expr Expr Expr
  | EqExpr Expr Expr
  | NeqExpr Expr Expr
  | LtEq Expr Expr
  | GtEq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | ShiftL Expr Expr
  | ShiftR Expr Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | PostfixInc Expr
  | PostfixDec Expr
  | PrefixInc Expr
  | PrefixDec Expr
  | PrimI Identifier
  | PrimC Literal
  | Assign Expr Op Expr
  | PrimExpr Expr
  | Neg Expr
  | AddrOf Expr
  | Not Expr
  | Plus Expr
  | Deref Expr
  | Compl Expr
  deriving (Eq, Show)

instance ToJSON Expr where
  toJSON (Or e0 e1) =
    object [ ("logical-or", array $ fmap toJSON [e0, e1])]
  toJSON (And e0 e1) =
    object [ ("logical-and", array $ fmap toJSON [e0, e1])]
  toJSON (BitwiseOr e0 e1) =
    object [ ("inclusive-or", array $ fmap toJSON [e0, e1])]
  toJSON (BitwiseAnd e0 e1) =
    object [ ("inclusive-and", array $ fmap toJSON [e0, e1])]
  toJSON (Xor e0 e1) =
    object [ ("exclusive-or", array $ fmap toJSON [e0, e1])]
  toJSON (CondExpr c e0 e1) =
    object [ ("cond-expr",
      object [ ("condition", toJSON c)
             , ("true-branch", toJSON e0)
             , ("false-branch", toJSON e1)
             ])
           ]
  toJSON (EqExpr e0 e1) =
    object [ ("equal", array $ fmap toJSON [e0, e1])]
  toJSON (NeqExpr e0 e1) =
    object [ ("not-equal", array $ fmap toJSON [e0, e1])]
  toJSON (LtEq e0 e1) =
    object [ ("lt-equal", array $ fmap toJSON [e0, e1])]
  toJSON (GtEq e0 e1) =
    object [ ("gt-equal", array $ fmap toJSON [e0, e1])]
  toJSON (Gt e0 e1) =
    object [ ("gt", array $ fmap toJSON [e0, e1])]
  toJSON (Lt e0 e1) =
    object [ ("lt", array $ fmap toJSON [e0, e1])]
  toJSON (ShiftL e0 e1) =
    object [ ("shiftl", array $ fmap toJSON [e0, e1])]
  toJSON (ShiftR e0 e1) =
    object [ ("shiftr", array $ fmap toJSON [e0, e1])]
  toJSON (Add e0 e1) =
    object [ ("add", array $ fmap toJSON [e0, e1])]
  toJSON (Sub e0 e1) =
    object [ ("sub", array $ fmap toJSON [e0, e1])]
  toJSON (Mul e0 e1) =
    object [ ("mul", array $ fmap toJSON [e0, e1])]
  toJSON (Div e0 e1) =
    object [ ("div", array $ fmap toJSON [e0, e1])]
  toJSON (Mod e0 e1) =
    object [ ("mod", array $ fmap toJSON [e0, e1])]
  toJSON (PostfixInc e) =
    object [ ("postfix-inc", toJSON e) ]
  toJSON (PostfixDec e) =
    object [ ("postfix-dec", toJSON e) ]
  toJSON (PrefixInc e) =
    object [ ("prefix-inc", toJSON e) ]
  toJSON (PrefixDec e) =
    object [ ("prefix-dec", toJSON e) ]
  toJSON (PrimI i) =
    object [ ("primary-expr-identifier", toJSON i) ]
  toJSON (PrimC c) =
    object [ ("primary-expr-constant", toJSON c) ]
  toJSON (Assign e0 op e1) =
    object [ ("assign",
      object [ ("lhs", toJSON e0)
             , ("operator", toJSON op)
             , ("rhs", toJSON e1)
             ])
           ]
  toJSON (PrimExpr e) =
    object [ ("primary-expr-expr", toJSON e) ]
  toJSON (Neg e) =
    object [ ("neg", toJSON e) ]
  toJSON (AddrOf e) =
    object [ ("address-of", toJSON e) ]
  toJSON (Not e) =
    object [ ("not", toJSON e) ]
  toJSON (Plus e) =
    object [ ("plus", toJSON e) ]
  toJSON (Deref e) =
    object [ ("dereference", toJSON e) ]
  toJSON (Compl e) =
    object [ ("bitwise-complement", toJSON e) ]

data Op
  = Equal
  | MulEq
  | DivEq
  | ModEq
  | AddEq
  | SubEq
  | ShLEq
  | ShREq
  | AndEq
  | XorEq
  | OrEq
  deriving (Eq, Show)

instance ToJSON Op where
  toJSON x = String $ T.toLower $ pack $ show x

data Jump
  = Goto Identifier
  | Continue
  | Break
  | Return (Maybe Expr)
  deriving (Eq, Show)

instance ToJSON Jump where
  toJSON (Goto ident)      = object [ ("goto", toJSON ident) ]
  toJSON Break             = String "break"
  toJSON Continue          = String "continue"
  toJSON (Return Nothing)  = String "return-void"
  toJSON (Return (Just e)) = object [ ("return", toJSON e) ]

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
