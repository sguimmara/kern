{-# LANGUAGE OverloadedStrings #-}
module Kern.AST.Representation where

import Data.Text as T

import Kern.AST

import Data.Yaml

-- instance ToJSON Linkage where
--   toJSON External = String "extern"
--   toJSON Internal = String "intern"

-- instance ToJSON Constness where
--   toJSON Constant = String "const"
--   toJSON Mutable = String "mut"

-- instance ToJSON Volatility where
--   toJSON Volatile = String "volatile"
--   toJSON NonVolatile = String "nonvolatile"

-- instance ToJSON Indirection where
--   toJSON Direct = String "direct"
--   toJSON (Indirect p) = object [("indirect", toJSON p)]

-- instance ToJSON ExternalType where
--   toJSON (ExternalType dt l c v i) =
--     object [ ("linkage", toJSON l)
--            , ("constness", toJSON c)
--            , ("volatility", toJSON v)
--            , ("indirection", toJSON i)
--            , ("datatype", toJSON dt)
--            ]

-- instance ToJSON InternalType where
--   toJSON (InternalType dt c v i) =
--     object [ ("constness", toJSON c)
--            , ("volatility", toJSON v)
--            , ("indirection", toJSON i)
--            , ("datatype", toJSON dt)
--            ]

-- instance ToJSON ParameterType where
--   toJSON (ParameterType dt c v i) =
--     object [ ("constness", toJSON c)
--            , ("volatility", toJSON v)
--            , ("indirection", toJSON i)
--            , ("datatype", toJSON dt)
--            ]

-- instance ToJSON Pointer where
--   toJSON (Pointer p) = String "*"

-- instance ToJSON Identifier where
--   toJSON (Ident t) = String t

-- instance ToJSON Parameter where
--   toJSON (Parameter ty ident) =
--     object [ ("type", toJSON ty)
--            , ("id", toJSON ident)]

-- instance ToJSON Initializer where
--   toJSON (InitExpr e) = object [ ("assignment-expr", toJSON e) ]

-- instance ToJSON LocalVariable where
--   toJSON (LocalVariable ty ident ini) =
--     object [ ("type", toJSON ty)
--            , ("id", toJSON ident)
--            , ("initializer", toJSON ini)
--            ]

-- instance ToJSON Type where
--   toJSON (Type dt l c v) =
--     object [ ("linkage", toJSON l)
--            , ("constness", toJSON c)
--            , ("volatility", toJSON v)
--            , ("datatype", toJSON dt)
--            ]

-- instance ToJSON GlobalVar where
--   toJSON (GlobalVar ident ty ini) =
--     object [ ("type", toJSON ty)
--            , ("id", toJSON ident)
--            , ("initializer", toJSON ini)
--            ]

-- instance ToJSON FunctionPrototype where
--   toJSON (Prototype ty ident ps) =
--     object [ ("return-type", toJSON ty)
--            , ("id", toJSON ident)]

-- instance ToJSON Statement where
--   toJSON (Jump j) = object [ ("jump-statement", toJSON j) ]
--   toJSON (ExprStmt Nothing) = String "empty-expr-statement"
--   toJSON (ExprStmt (Just e)) = object [ ("expr-statement", toJSON e) ]

-- instance ToJSON FunctionDefinition where
--   toJSON (Function ty ident ps body) =
--     object [ ("return-type", toJSON ty)
--            , ("id", toJSON ident)
--            , ("parameters", array $ fmap toJSON ps)
--            , ("body", toJSON body)]

-- instance ToJSON Expr where
--   toJSON (Or e0 e1) =
--     object [ ("logical-or", array $ fmap toJSON [e0, e1])]
--   toJSON (And e0 e1) =
--     object [ ("logical-and", array $ fmap toJSON [e0, e1])]
--   toJSON (BitwiseOr e0 e1) =
--     object [ ("inclusive-or", array $ fmap toJSON [e0, e1])]
--   toJSON (BitwiseAnd e0 e1) =
--     object [ ("inclusive-and", array $ fmap toJSON [e0, e1])]
--   toJSON (Xor e0 e1) =
--     object [ ("exclusive-or", array $ fmap toJSON [e0, e1])]
--   toJSON (CondExpr c e0 e1) =
--     object [ ("cond-expr",
--       object [ ("condition", toJSON c)
--              , ("true-branch", toJSON e0)
--              , ("false-branch", toJSON e1)
--              ])
--            ]
--   toJSON (EqExpr e0 e1) =
--     object [ ("equal", array $ fmap toJSON [e0, e1])]
--   toJSON (NeqExpr e0 e1) =
--     object [ ("not-equal", array $ fmap toJSON [e0, e1])]
--   toJSON (LtEq e0 e1) =
--     object [ ("lt-equal", array $ fmap toJSON [e0, e1])]
--   toJSON (GtEq e0 e1) =
--     object [ ("gt-equal", array $ fmap toJSON [e0, e1])]
--   toJSON (Gt e0 e1) =
--     object [ ("gt", array $ fmap toJSON [e0, e1])]
--   toJSON (Lt e0 e1) =
--     object [ ("lt", array $ fmap toJSON [e0, e1])]
--   toJSON (ShiftL e0 e1) =
--     object [ ("shiftl", array $ fmap toJSON [e0, e1])]
--   toJSON (ShiftR e0 e1) =
--     object [ ("shiftr", array $ fmap toJSON [e0, e1])]
--   toJSON (AddExpr e0 e1) =
--     object [ ("add", array $ fmap toJSON [e0, e1])]
--   toJSON (SubExpr e0 e1) =
--     object [ ("sub", array $ fmap toJSON [e0, e1])]
--   toJSON (MulExpr e0 e1) =
--     object [ ("mul", array $ fmap toJSON [e0, e1])]
--   toJSON (DivExpr e0 e1) =
--     object [ ("div", array $ fmap toJSON [e0, e1])]
--   toJSON (ModExpr e0 e1) =
--     object [ ("mod", array $ fmap toJSON [e0, e1])]
--   toJSON (PostfixInc e) =
--     object [ ("postfix-inc", toJSON e) ]
--   toJSON (PostfixDec e) =
--     object [ ("postfix-dec", toJSON e) ]
--   toJSON (PrefixInc e) =
--     object [ ("prefix-inc", toJSON e) ]
--   toJSON (PrefixDec e) =
--     object [ ("prefix-dec", toJSON e) ]
--   toJSON (PrimI i) =
--     object [ ("primary-expr-identifier", toJSON i) ]
--   toJSON (PrimC c) =
--     object [ ("primary-expr-constant", toJSON c) ]
--   toJSON (Assign e0 op e1) =
--     object [ ("assign",
--       object [ ("lhs", toJSON e0)
--              , ("operator", toJSON op)
--              , ("rhs", toJSON e1)
--              ])
--            ]
--   toJSON (Neg e) =
--     object [ ("neg", toJSON e) ]
--   toJSON (AddrOf e) =
--     object [ ("address-of", toJSON e) ]
--   toJSON (Not e) =
--     object [ ("not", toJSON e) ]
--   toJSON (Plus e) =
--     object [ ("plus", toJSON e) ]
--   toJSON (Deref e) =
--     object [ ("dereference", toJSON e) ]
--   toJSON (Compl e) =
--     object [ ("bitwise-complement", toJSON e) ]

-- instance ToJSON Op where
--   toJSON x = String $ T.toLower $ pack $ show x

-- instance ToJSON Jump where
--   toJSON (Goto ident)      = object [ ("goto", toJSON ident) ]
--   toJSON Break             = String "break"
--   toJSON Continue          = String "continue"
--   toJSON (Return Nothing)  = String "return-void"
--   toJSON (Return (Just e)) = object [ ("return", toJSON e) ]

-- instance ToJSON TranslationUnit where
--   toJSON (TranslationUnit decls) =
--     object [ ("translation-unit", array $ fmap toJSON decls) ]

-- instance ToJSON ExternalDeclaration where
--   toJSON (FunctionDefinition fd) =
--     object [ ("function-definition", toJSON fd) ]
--   toJSON (GlobalDeclaration g) =
--     object [ ("global-variable", toJSON g) ]
--   -- toJSON (FunctionPrototype fp) =
--   --   object [ ("function-declaration", toJSON fp) ]
--   --
-- instance ToJSON Literal where
--   toJSON (Int32Lit n) = object [ ("int32-literal", toJSON n) ]
--   toJSON (Int64Lit n) = object [ ("int64-literal", toJSON n) ]
--   toJSON (CharLit c) = object [ ("char-literal", toJSON c) ]
--   toJSON (Float32Lit f) = object [ ("float32-literal", toJSON f) ]
--   toJSON (Float64Lit f) = object [ ("float64-literal", toJSON f) ]

-- instance ToJSON Body where
--   toJSON (Body vars stms) =
--     object [ ("local-vars", array $ fmap toJSON vars)
--            , ("statements", array $ fmap toJSON stms)
--            ]
-- --
-- instance ToJSON DataType where
--   toJSON TyVoid = String "void"
--   toJSON TyByte = String "byte"
--   toJSON TyInt16 = String "i16"
--   toJSON TyInt32 = String "i32"
--   toJSON TyInt64 = String "i64"
--   toJSON TyUint16 = String "u16"
--   toJSON TyUint32 = String "u32"
--   toJSON TyUint64 = String "u64"
--   toJSON TyFloat32 = String "f32"
--   toJSON TyFloat64 = String "f64"