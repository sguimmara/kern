{-# LANGUAGE OverloadedStrings #-}
module Kern.AST.Pretty where

import           Data.Char
import           Data.Maybe
import           Data.List
import qualified Data.Text as T

import           Text.PrettyPrint
import           Text.PrettyPrint.HughesPJClass (Pretty, pPrint)

import           Kern.AST

instance Pretty DataType where
  pPrint TyVoid    = text "void"
  pPrint TyByte    = text "char"
  pPrint TyInt16   = text "short"
  pPrint TyInt32   = text "int"
  pPrint TyInt64   = text "long" <+> text "int"
  pPrint TyUint16  = text "unsigned" <+> text "short"
  pPrint TyUint32  = text "unsigned" <+> text "int"
  pPrint TyUint64  = text "unsigned" <+> text "long"
  pPrint TyFloat32 = text "float"
  pPrint TyFloat64 = text "double"

instance Pretty Identifier where
  pPrint (Id t) = text $ T.unpack t

instance Pretty Literal where
  pPrint (CChar c)    = quotes (char c)
  pPrint (CString s)  = doubleQuotes (text s)
  pPrint (CInt32 n)   = integer $ fromIntegral n
  pPrint (CInt64 n)   = (integer $ fromIntegral n) <> char 'L'
  pPrint (CFloat32 n) = float n <> char 'f'
  pPrint (CFloat64 n) = double n

instance Pretty StorCls where
  pPrint DefaultStorage = empty
  pPrint x = text $ map toLower (show x)

instance Pretty TypeQual where
  pPrint x = text $ map toLower (show x)

instance Pretty StructDecl where
  pPrint (StructDecl dt qs decltors) =
    (hsep $ map pPrint qs) <+> pPrint dt <+>
    (hsep $ punctuate comma (map pPrint decltors))

instance Pretty StructDecltor where
  pPrint (StructDecltor d) = pPrint d

instance Pretty Decltor where
  pPrint (Decltor p dd) = (maybe empty pPrint p) <+> pPrint dd

instance Pretty Pointer where
  pPrint (Pointer qs p) =
    char '*' <+> (hsep $ map pPrint qs) <>
    (maybe empty (\x -> space <> (pPrint x)) p)

instance Pretty DirectDecltor where
  pPrint (NameDecltor i) = pPrint i
  pPrint (ArrayDecltor i e) =
    pPrint i <> char '[' <> (maybe empty pPrint e) <> char ']'
  pPrint (FuncDecltor i ps) =
    pPrint i <> char '(' <>
    (hsep $ punctuate comma (map pPrint ps)) <> char ')'

instance Pretty Declaration where
  pPrint (Declaration dt sc qs is) =
    pPrintQualifiers dt sc qs <+>
    (hsep $ punctuate comma (map pPrint is)) <> semi

pPrintQualifiers dt sc qs =
  (hsep $ map pPrint (sort qs)) <+>
  pPrint sc <+>
  pPrint dt

instance Pretty InitDecltor where
  pPrint (InitDecltor d Nothing) = pPrint d
  pPrint (InitDecltor d (Just x)) = error "unsupported"

instance Pretty ParamDecl where
  pPrint (ParamDecl dt sc qs d) = pPrintQualifiers dt sc qs <+> pPrint d

instance Pretty FunctionDefinition where
  pPrint (FuncDef dt sc qs d cs) =
    pPrintQualifiers dt sc qs <+>
    pPrint d <+> pPrint cs

instance Pretty CompoundStatement where
  pPrint (CompoundStmt ds ss) =
    lbrace $+$
    nest 4 (vcat $ map pPrint ds) $+$
    nest 4 (vcat $ map pPrint ss) $+$
    rbrace

-- instance Pretty Parameter where
--   pretty (Parameter ty i) = pretty ty <+> pretty i

-- instance Pretty Initializer where
--   pretty (InitExpr expr) = pretty expr

-- instance Pretty LocalVariable where
--   pretty (LocalVariable ty i Nothing)    = pretty ty <+> pretty i <> semi
--   pretty (LocalVariable ty i (Just ini)) =
--     pretty ty <+> pretty i <+> char '=' <+> pretty ini <> semi

-- instance Pretty Type where
--   pretty (Type d l c v) = pretty c <+> pretty v <+> pretty l <+> pretty d

-- instance Pretty GlobalVar where
--   pretty (GlobalVar i ty Nothing)    = pretty ty <+> pretty i <> semi
--   pretty (GlobalVar i ty (Just ini)) =
--     pretty ty <+> pretty i <+> char '=' <+> pretty ini <> semi

-- instance Pretty FunctionPrototype where
--   pretty (Prototype ty i ps) =
--     pretty ty <+> pretty i <>
--     char '(' <>
--     (hsep (punctuate comma (map pretty ps))) <>
--     char ')' <> semi

-- instance Pretty FunctionDefinition where
--   pretty (Function ty i ps b) =
--     pretty ty <+> pretty i <>
--     char '(' <>
--     (hsep (punctuate comma(map pretty ps))) <>
--     char ')' $+$
--     lbrace $+$
--     nest 4 (vcat $ map pretty (bodyLocals b)) $+$
--     nest 4 (vcat $ map pretty (bodyStmts b)) $+$
--     rbrace

-- instance Pretty ExternalDeclaration where
--   pretty (FunctionDefinition f) = pretty f
--   pretty (GlobalDeclaration d) = pretty d

-- instance Pretty TranslationUnit where
--   pretty (TranslationUnit decls) =
--     vcat (map pretty decls)

-- instance Pretty Statement where
--   pretty (Jump j)            = pretty j
--   pretty (ExprStmt Nothing)  = semi
--   pretty (ExprStmt (Just e)) = pretty e <> semi

-- instance Pretty Jump where
--   pretty (Goto i)          = text "goto" <+> pretty i <> semi
--   pretty Continue          = text "continue" <> semi
--   pretty Break             = text "break" <> semi
--   pretty (Return Nothing)  = text "return" <> semi
--   pretty (Return (Just e)) = text "return" <+> pretty e <> semi

instance Pretty Expr where
  pPrint (Or e0 e1)         = pPrint e0 <+> text "||" <+> pPrint e1
  pPrint (And e0 e1)        = pPrint e0 <+> text "&&" <+> pPrint e1
  pPrint (BitwiseOr e0 e1)  = pPrint e0 <+> text "|" <+> pPrint e1
  pPrint (BitwiseAnd e0 e1) = pPrint e0 <+> text "&" <+> pPrint e1
  pPrint (Xor e0 e1)        = pPrint e0 <+> text "^" <+> pPrint e1
  pPrint (CondExpr cnd t f) =
    pPrint cnd <+> char '?' <+> pPrint t <+> char ':' <+> pPrint f
  pPrint (EqExpr e0 e1)     = pPrint e0 <+> text "==" <+> pPrint e1
  pPrint (NeqExpr e0 e1)    = pPrint e0 <+> text "!=" <+> pPrint e1
  pPrint (Lt e0 e1)         = pPrint e0 <+> text "<" <+> pPrint e1
  pPrint (Gt e0 e1)         = pPrint e0 <+> text ">" <+> pPrint e1
  pPrint (GtEq e0 e1)       = pPrint e0 <+> text ">=" <+> pPrint e1
  pPrint (LtEq e0 e1)       = pPrint e0 <+> text "<=" <+> pPrint e1
  pPrint (ShiftL e0 e1)     = pPrint e0 <+> text "<<" <+> pPrint e1
  pPrint (ShiftR e0 e1)     = pPrint e0 <+> text ">>" <+> pPrint e1
  pPrint (AddExpr e0 e1)    = pPrint e0 <+> text "+" <+> pPrint e1
  pPrint (SubExpr e0 e1)    = pPrint e0 <+> text "-" <+> pPrint e1
  pPrint (MulExpr e0 e1)    = pPrint e0 <+> text "*" <+> pPrint e1
  pPrint (DivExpr e0 e1)    = pPrint e0 <+> text "/" <+> pPrint e1
  pPrint (ModExpr e0 e1)    = pPrint e0 <+> text "%" <+> pPrint e1
  pPrint (PostfixInc e)     = pPrint e <> text "++"
  pPrint (PostfixDec e)     = pPrint e <> text "--"
  pPrint (PrefixInc e)      = text "++" <> pPrint e
  pPrint (PrefixDec e)      = text "--" <> pPrint e
  pPrint (PrimI i)          = pPrint i
  pPrint (PrimC c)          = pPrint c
  pPrint (Assign lh op rh)  = pPrint lh <+> pPrint op <+> pPrint rh
  pPrint (Neg e)            = char '-' <> pPrint e
  pPrint (AddrOf e)         = char '&' <> pPrint e
  pPrint (Not e)            = char '!' <> pPrint e
  pPrint (Plus e)           = char '+' <> pPrint e
  pPrint (Deref e)          = char '*' <> pPrint e
  pPrint (Compl e)          = char '~' <> pPrint e

instance Pretty Op where
  pPrint Equal = char '='
  pPrint MulEq = text "*="
  pPrint DivEq = text "/="
  pPrint ModEq = text "%="
  pPrint AddEq = text "+="
  pPrint SubEq = text "-="
  pPrint ShLEq = text "<<="
  pPrint ShREq = text ">>="
  pPrint AndEq = text "&="
  pPrint XorEq = text "^="
  pPrint OrEq  = text "|="

instance Pretty Statement where
  pPrint (Goto i) = text "goto" <+> pPrint i <> semi
  pPrint Break = text "break;"
  pPrint Continue = text "continue;"
  pPrint (Return e) = text "return" <+> (maybe empty pPrint e) <> semi
  pPrint (ExprStmt e) = (maybe empty pPrint e) <> semi
  pPrint (Case e s) = text "case" <+> pPrint e <+> char ':' <+> pPrint s
  pPrint (DefaultStmt s) = text "default:" <+> pPrint s
  pPrint (LabelStmt i s) = pPrint i <> char ':' <+> pPrint s