module Kern.ASTOld where

-- import Data.Int
-- import Data.Text as T

-- import Kern.Core

-- newtype TranslationUnit =
--   TranslationUnit [ExternalDeclaration]
--   deriving (Eq, Show)

-- data ExternalDeclaration
--   = FunctionDefinition FunctionDefinition
--   | GlobalDeclaration GlobalVar
--   deriving (Eq, Show)

-- data FunctionDefinition =
--   Function { funcType :: ExternalType
--            , funcName :: Identifier
--            , funcParams :: Params
--            , funcBody :: Body
--            }
--   deriving (Eq, Show)

-- data Declaration = Declaration [DeclarationSpecifier] [InitDeclarator]
--                    deriving (Eq, Show)

-- data Declarator = Declarator (Maybe Pointer) DirectDeclarator
--                   deriving (Eq, Show)

-- data DirectDeclarator
--   = DeclIdentifier Identifier
--   | DeclParenthese Declarator
--   | DeclArray      DirectDeclarator (Maybe Expr)
--   | DeclParams     DirectDeclarator [Parameter]
--   deriving (Eq, Show)

-- data InitDeclarator = InitDeclarator Declarator (Maybe Initializer)
--                       deriving (Eq, Show)

-- data DataType
--   = TyVoid
--   | TyByte
--   | TyInt16
--   | TyInt32
--   | TyInt64
--   | TyUint16
--   | TyUint32
--   | TyUint64
--   | TyFloat32
--   | TyFloat64
--   | Array DataType
--   deriving (Eq, Show)

-- data Literal
--   = Int32Lit Int32
--   | Int64Lit Int64
--   | CharLit Char
--   | Float32Lit Float
--   | Float64Lit Double
--   deriving (Eq, Show)

-- data DeclarationSpecifier
--   = StorageClass StorageClass
--   | TypeSpecifier TypeSpecifier
--   | TypeQualifier TypeQualifier
--   deriving (Eq, Show)

-- data StorageClass
--   = Auto
--   | Register
--   | Static
--   | Extern
--   | Typedef
--   deriving (Eq, Show, Ord)

-- data TypeSpecifier
--   = SpecVoid
--   | SpecChar
--   | SpecShort
--   | SpecInt
--   | SpecLong
--   | SpecFloat
--   | SpecDouble
--   | SpecSigned
--   | SpecUnsigned
--   deriving (Eq, Show, Ord)

-- data TypeQualifier
--   = ConstQualifier
--   | VolatileQualifier
--   deriving (Eq, Show, Ord)

-- data Linkage
--   = External
--   | Internal
--   deriving (Eq, Show, Enum, Bounded)

-- data Constness
--   = Constant
--   | Mutable
--   deriving (Eq, Show, Enum, Bounded)

-- data Volatility
--   = Volatile
--   | NonVolatile
--    deriving (Eq, Show, Enum, Bounded)

-- data Indirection
--   = Direct
--   | Indirect Pointer
--   deriving (Eq, Show)

-- data ExternalType =
--   ExternalType DataType
--                Linkage
--                Constness
--                Volatility
--                Indirection
--   deriving (Eq, Show)

-- data InternalType =
--   InternalType DataType
--                Constness
--                Volatility
--                Indirection
--   deriving (Eq, Show)

-- data ParameterType =
--   ParameterType DataType
--                 Constness
--                 Volatility
--                 Indirection
--   deriving (Eq, Show)

-- newtype Pointer =
--   Pointer (Maybe Pointer)
--   deriving (Eq, Show)

-- newtype Identifier = Ident Text deriving (Eq, Show)

-- type Params = [Parameter]
-- data Parameter = Parameter ParameterType Identifier deriving (Eq, Show)

-- data Initializer
--   = InitExpr Expr
--   deriving (Eq, Show)

-- data LocalVariable =
--   LocalVariable InternalType Identifier (Maybe Initializer)
--   deriving (Eq, Show)

-- data Type = Type DataType Linkage Constness Volatility
--             deriving (Eq, Show)

-- data GlobalVar = GlobalVar Identifier Type (Maybe Initializer)
--                  deriving (Eq, Show)

-- data LocalVar = LocalVar Identifier Type (Maybe Initializer)
--                 deriving (Eq, Show)

-- data FunctionPrototype = Prototype ExternalType Identifier Params
--                          deriving (Eq, Show)

-- data Statement
--   = Jump Jump
--   | ExprStmt (Maybe Expr)
--   deriving (Eq, Show)

-- data Body = Body { bodyLocals :: [LocalVariable]
--                  , bodyStmts :: [Statement]
--                  }
--             deriving (Eq, Show)

-- data Expr
--   = Or Expr Expr
--   | And Expr Expr
--   | BitwiseOr Expr Expr
--   | BitwiseAnd Expr Expr
--   | Xor Expr Expr
--   | CondExpr Expr Expr Expr
--   | EqExpr Expr Expr
--   | NeqExpr Expr Expr
--   | LtEq Expr Expr
--   | GtEq Expr Expr
--   | Lt Expr Expr
--   | Gt Expr Expr
--   | ShiftL Expr Expr
--   | ShiftR Expr Expr
--   | AddExpr Expr Expr
--   | SubExpr Expr Expr
--   | MulExpr Expr Expr
--   | DivExpr Expr Expr
--   | ModExpr Expr Expr
--   | PostfixInc Expr
--   | PostfixDec Expr
--   | PrefixInc Expr
--   | PrefixDec Expr
--   | PrimI Identifier
--   | PrimC Literal
--   | Assign Expr Op Expr
--   | Neg Expr
--   | AddrOf Expr
--   | Not Expr
--   | Plus Expr
--   | Deref Expr
--   | Compl Expr
--   deriving (Eq, Show)

-- data Op
--   = Equal
--   | MulEq
--   | DivEq
--   | ModEq
--   | AddEq
--   | SubEq
--   | ShLEq
--   | ShREq
--   | AndEq
--   | XorEq
--   | OrEq
--   deriving (Eq, Show, Enum, Bounded)

-- data Jump
--   = Goto Identifier
--   | Continue
--   | Break
--   | Return (Maybe Expr)
--   deriving (Eq, Show)





