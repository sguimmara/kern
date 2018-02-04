{-# LANGUAGE OverloadedStrings #-}
module Kern.AST.Pretty where

import           Data.Char
import qualified Data.Text as T hiding (map)
import           Text.PrettyPrint

import Kern.Core
import Kern.AST

instance Pretty Identifier where
  pretty (Ident t) = text $ T.unpack t

instance Pretty Literal where
  pretty (Int32Lit i32) = integer $ fromIntegral i32
  pretty (Int64Lit i64) = (integer $ fromIntegral i64) <> char 'L'
  pretty (CharLit c) = quotes (char c)
  pretty (Float32Lit f32) = float f32 <> char 'f'
  pretty (Float64Lit f64) = double f64

instance Pretty DataType where
  pretty dt =
    case dt of
      TyVoid -> text "void"
      TyByte -> text "char"
      TyInt16 -> text "short int"
      TyInt32 -> text "int"
      TyInt64 -> text "long int"
      TyUint16 -> text "unsigned short int"
      TyUint32 -> text "unsigned int"
      TyUint64 -> text "unsigned long int"
      TyFloat32 -> text "float"
      TyFloat64 -> text "double"

instance Pretty StorageClass where
  pretty x = text $ map toLower (show x)

instance Pretty Volatility where
  pretty Volatile = text "volatile"
  pretty NonVolatile = empty

instance Pretty Linkage where
  pretty External = empty
  pretty Internal = text "static"

instance Pretty Constness where
  pretty Constant = text "const"
  pretty Mutable  = empty

instance Pretty Indirection where
  pretty Direct = empty
  pretty (Indirect p) = pretty p

instance Pretty Pointer where
  pretty (Pointer Nothing) = char '*'
  pretty (Pointer (Just p)) = char '*' <> pretty p

instance Pretty ExternalType where
  pretty (ExternalType dt l c v i) =
    hsep $
      [ pretty c
      , pretty v
      , pretty l
      , pretty dt
      , pretty i]

instance Pretty InternalType where
  pretty (InternalType dt c v i) =
    hsep $
      [ pretty c
      , pretty v
      , pretty dt
      , pretty i]

instance Pretty ParameterType where
  pretty (ParameterType dt c v i) =
    hsep $
      [ pretty c
      , pretty v
      , pretty dt
      , pretty i]

instance Pretty Parameter where
  pretty (Parameter ty i) = pretty ty <+> pretty i

instance Pretty Initializer where
  pretty (InitExpr expr) = pretty expr

instance Pretty LocalVariable where
  pretty (LocalVariable ty i Nothing)    = pretty ty <+> pretty i <> semi
  pretty (LocalVariable ty i (Just ini)) =
    pretty ty <+> pretty i <+> char '=' <+> pretty ini <> semi

instance Pretty FunctionPrototype where
  pretty (Prototype ty i ps) =
    pretty ty <+> pretty i <>
    char '(' <>
    (hsep (punctuate comma (map pretty ps))) <>
    char ')' <> semi

instance Pretty FunctionDefinition where
  pretty (Function ty i ps b) =
    pretty ty <+> pretty i <>
    char '(' <>
    (hsep (punctuate comma(map pretty ps))) <>
    char ')' $+$
    lbrace $+$
    nest 4 (vcat $ map pretty (bodyLocals b)) $+$
    nest 4 (vcat $ map pretty (bodyStmts b)) $+$
    rbrace

instance Pretty Statement where
  pretty (Jump j)            = pretty j
  pretty (ExprStmt Nothing)  = semi
  pretty (ExprStmt (Just e)) = pretty e <> semi

instance Pretty Jump where
  pretty (Goto i)          = text "goto" <+> pretty i <> semi
  pretty Continue          = text "continue" <> semi
  pretty Break             = text "break" <> semi
  pretty (Return Nothing)  = text "return" <> semi
  pretty (Return (Just e)) = text "return" <+> pretty e <> semi

instance Pretty Expr where
  pretty (Or e0 e1)         = pretty e0 <+> text "||" <+> pretty e1
  pretty (And e0 e1)        = pretty e0 <+> text "&&" <+> pretty e1
  pretty (BitwiseOr e0 e1)  = pretty e0 <+> text "|" <+> pretty e1
  pretty (BitwiseAnd e0 e1) = pretty e0 <+> text "&" <+> pretty e1
  pretty (Xor e0 e1)        = pretty e0 <+> text "^" <+> pretty e1
  pretty (CondExpr cnd t f) =
    pretty cnd <+> char '?' <+> pretty t <+> char ':' <+> pretty f
  pretty (EqExpr e0 e1)     = pretty e0 <+> text "==" <+> pretty e1
  pretty (NeqExpr e0 e1)    = pretty e0 <+> text "!=" <+> pretty e1
  pretty (Lt e0 e1)         = pretty e0 <+> text "<" <+> pretty e1
  pretty (Gt e0 e1)         = pretty e0 <+> text ">" <+> pretty e1
  pretty (GtEq e0 e1)       = pretty e0 <+> text ">=" <+> pretty e1
  pretty (LtEq e0 e1)       = pretty e0 <+> text "<=" <+> pretty e1
  pretty (ShiftL e0 e1)     = pretty e0 <+> text "<<" <+> pretty e1
  pretty (ShiftR e0 e1)     = pretty e0 <+> text ">>" <+> pretty e1
  pretty (AddExpr e0 e1)    = pretty e0 <+> text "+" <+> pretty e1
  pretty (SubExpr e0 e1)    = pretty e0 <+> text "-" <+> pretty e1
  pretty (MulExpr e0 e1)    = pretty e0 <+> text "*" <+> pretty e1
  pretty (DivExpr e0 e1)    = pretty e0 <+> text "/" <+> pretty e1
  pretty (ModExpr e0 e1)    = pretty e0 <+> text "%" <+> pretty e1
  pretty (PostfixInc e)     = pretty e <> text "++"
  pretty (PostfixDec e)     = pretty e <> text "--"
  pretty (PrefixInc e)      = text "++" <> pretty e
  pretty (PrefixDec e)      = text "--" <> pretty e
  pretty (PrimI i)          = pretty i
  pretty (PrimC c)          = pretty c
  pretty (Assign lh op rh)  = pretty lh <+> pretty op <+> pretty rh
  pretty (PrimExpr e)       = pretty e
  pretty (Neg e)            = char '-' <> pretty e
  pretty (AddrOf e)         = char '&' <> pretty e
  pretty (Not e)            = char '!' <> pretty e
  pretty (Plus e)           = char '+' <> pretty e
  pretty (Deref e)          = char '*' <> pretty e
  pretty (Compl e)          = char '~' <> pretty e

instance Pretty Op where
  pretty Equal = char '='
  pretty MulEq = text "*="
  pretty DivEq = text "/="
  pretty ModEq = text "%="
  pretty AddEq = text "+="
  pretty SubEq = text "-="
  pretty ShLEq = text "<<="
  pretty ShREq = text ">>="
  pretty AndEq = text "&="
  pretty XorEq = text "^="
  pretty OrEq  = text "|="