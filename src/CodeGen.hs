module CodeGen
    ( emit
    , Emittable
    , Register (..)
    , Operand (..)
    , Asm (..)
    , Size (..)
    ) where

import AST

import Data.Text    (pack, unpack)
import Data.Char    (toLower)
import Data.List    (intercalate)

------------------------------------------------------------------------
--- Constants & utilities ----------------------------------------------
------------------------------------------------------------------------

eax = OpReg Eax
rbp = OpReg Rbp
rsp = OpReg Rsp

popq = Pop Q
pushq = Push Q
popl = Pop L
pushl = Push L

movq = Mov Q
movl = Mov L

s_text      = Section "text" []
s_global n  = Section "globl" [n]
s_file n    = Section "file" [n]
s_type n t  = Section "type" [n, show t]

getId :: Identifier -> String
getId (Ident i) = unpack i

------------------------------------------------------------------------
--- Emitters -----------------------------------------------------------
------------------------------------------------------------------------

data Asm =
         -- Misc ---------------------------------------------------
           Label String
         | Section String [String]
         -- Instructions------------------------------------------------
         | Ret
         | Mov Size Operand Operand
         | Pop Size Operand
         | Push Size Operand
           deriving (Eq)

class Emittable a where
    emit :: a -> [Asm]


instance Emittable Return where
    emit (ReturnLit lit) = [ movl (emitLiteral lit) eax
                           , popq rbp
                           , Ret
                           ]

instance Emittable Statement where
    emit (ReturnStmt ret) = emit ret

instance Emittable Function where
    emit fun = fprolog fun ++ fbody fun

fprolog :: Function -> [Asm]
fprolog (Func _ name _ _) = let n = getId name in
                            [ s_global n
                            , s_type n TyFunction
                            , Label n
                            ]

fbody :: Function -> [Asm]
fbody (Func _ _ _ stmts) = [ pushq rbp
                           , movq rsp rbp
                           ] ++ concatMap emit stmts

instance Emittable TranslationUnit where
    emit (TranslationUnit file funcs) = [ s_file file
                                        , s_text ] ++ concatMap emit funcs

emitLiteral (IntLit x) = OpValue x

data Register = Eax | Rbp | Rsp
                deriving (Show, Eq)

data Operand = OpValue Int
             | OpReg Register
               deriving (Eq)

instance Show Operand where
    show (OpValue x) = "$" ++ show x
    show (OpReg reg) = "%" ++ (map toLower (show reg))

data ObjType = TyFunction
               deriving (Eq)

instance Show ObjType where
    show TyFunction = "@function"

data Size = L | Q
            deriving (Eq)

instance Show Size where
    show L = "l"
    show Q = "q"
