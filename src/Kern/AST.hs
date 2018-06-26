module Kern.AST where

import           Data.Int
import           Data.Text
import           Data.Char

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
  deriving (Eq, Show, Enum, Bounded)

data Literal
  = StringLiteral StrLit
  | NumLiteral NumLit
  deriving (Eq, Show)

data StrLit = StrLit !String
  deriving (Eq, Show)

data NumLit
  = CChar !Char
  | CFloat32 !Float
  | CFloat64 !Double
  | CInt64 !Int64
  | CInt32 !Int32
  deriving (Eq, Show)

instance Num NumLit where
  (+) (CChar c1) (CChar c2) = CChar  $ chr $ (ord c1) + (ord c2)

  (+) (CInt64 a) (CInt64 b) = CInt64 $ a + b
  (+) (CInt32 a) (CInt32 b) = CInt32 $ a + b
  (+) (CInt64 a) (CInt32 b) = CInt64 $ a + fromIntegral b
  (+) (CInt32 a) (CInt64 b) = CInt64 $ fromIntegral a + b

  (+) (CFloat64 a) (CFloat64 b) = CFloat64 $ a + b
  (+) (CFloat32 a) (CFloat32 b) = CFloat32 $ a + b
  (+) (CFloat64 a) (CFloat32 b) = CFloat64 $ (realToFrac a) + (realToFrac b)
  (+) (CFloat32 a) (CFloat64 b) = CFloat64 $ (realToFrac a) + (realToFrac b)


data StorCls
  = Auto
  | Register
  | Static
  | Extern
  | Typedef
  | DefaultStorage
  deriving (Eq, Show, Enum, Bounded)

data TypeSpec
  = SpecVoid
  | SpecChar
  | SpecInt
  | SpecLong
  | SpecShort
  | SpecFloat
  | SpecDouble
  | SpecSigned
  | SpecUnsigned
  | Struct (Maybe Identifier) [StructDecl]
  | Union (Maybe Identifier) [StructDecl]
  | SpecEnum (Maybe Identifier) [Enumerator]
  | TypedefId Identifier
  deriving (Eq, Show)

data TypeQual
  = Const
  | Volatile
  deriving (Eq, Show, Enum, Bounded, Ord)

newtype Identifier
  = Id Text
  deriving (Eq, Show)

data StructDecl
  = StructDecl DataType [TypeQual] [StructDecltor]
  deriving (Eq, Show)

data StructDecltor
  = StructDecltor Decltor
  deriving (Eq, Show)

data Enumerator
  = Enumerator Identifier
  deriving (Eq, Show)

data Decltor
  = Decltor (Maybe Pointer) DirectDecltor
  deriving (Eq, Show)

data Pointer
  = Pointer [TypeQual] (Maybe Pointer)
  deriving (Eq, Show)

data DirectDecltor
  = NameDecltor Identifier
  | ArrayDecltor Identifier (Maybe Expr)
  | FuncDecltor Identifier [ParamDecl]
  deriving (Eq, Show)

data Declaration
  = Declaration DataType StorCls [TypeQual] [InitDecltor]
  deriving (Eq, Show)

data InitDecltor
  = InitDecltor Decltor (Maybe Initializer)
  deriving (Eq, Show)

data Initializer
  = Initializer
  deriving (Eq, Show)

data ParamDecl
  = ParamDecl DataType StorCls [TypeQual] Decltor
  deriving (Eq, Show)

data FunctionDefinition
  = FuncDef DataType StorCls [TypeQual] Decltor CompoundStatement
  deriving (Eq, Show)

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
  | AddExpr Expr Expr
  | SubExpr Expr Expr
  | MulExpr Expr Expr
  | DivExpr Expr Expr
  | ModExpr Expr Expr
  | PostfixInc Expr
  | PostfixDec Expr
  | PrefixInc Expr
  | PrefixDec Expr
  | PrimI Identifier
  | PrimC Literal
  | Assign Expr Op Expr
  | Neg Expr
  | AddrOf Expr
  | Not Expr
  | Plus Expr
  | Deref Expr
  | Compl Expr
  deriving (Eq, Show)

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
  deriving (Eq, Show, Enum, Bounded)

data CompoundStatement
  = CompoundStmt [Declaration] [Statement]
  deriving (Eq, Show)

data Statement
  = Goto Identifier
  | Continue
  | Break
  | Return (Maybe Expr)
  | ExprStmt (Maybe Expr)
  | Case Expr Statement
  | DefaultStmt Statement
  | LabelStmt Identifier Statement
  deriving (Eq, Show)