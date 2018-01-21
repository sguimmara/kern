module CodeGen
    ( emit
    , toAssembly
    , Emittable
    , Register (..)
    , Operand (..)
    , Asm (..)
    ) where

import AST

import Data.Text    (pack, unpack)


------------------------------------------------------------------------
--- Constants ----------------------------------------------------------
------------------------------------------------------------------------

eax = OpReg Eax
rbp = OpReg Rbp
rsp = OpReg Rsp

class Emittable a where
    emit :: a -> [Asm]


instance Emittable Return where
    emit (ReturnLit lit) = [ Mov L (emitLiteral lit) eax
                           , Pop Q rbp
                           , Ret
                           ]

instance Emittable Statement where
    emit (ReturnStmt ret) = emit ret

instance Emittable Function where
    emit fun = fprolog fun ++ fbody fun

fprolog :: Function -> [Asm]
fprolog (Func _ name _ _) = let asmname = unpack name in
                            [ Global asmname
                            , Type asmname TyFunction
                            , Label asmname
                            ]

fbody :: Function -> [Asm]
fbody (Func _ _ _ stmts) = [ Push Q rbp
                           , Mov Q rsp rbp
                           ] ++ concatMap emit stmts

instance Emittable TranslationUnit where
    emit (TranslationUnit file funcs) = [ SFile file
                                        , SText ] ++ concatMap emit funcs

emitLiteral (IntLit x) = OpValue x

data Register = Eax | Rbp | Rsp
                deriving (Show, Eq)

data Operand = OpValue Int
             | OpReg Register
               deriving (Eq)

instance Show Operand where
    show (OpValue x) = "$" ++ show x
    show (OpReg reg) = "%" ++ case reg of
                                     Eax -> "eax"
                                     Rbp -> "rbp"
                                     Rsp -> "rsp"

data ObjType = TyFunction
               deriving (Eq)

instance Show ObjType where
    show TyFunction = "@function"

data Size = L | Q
            deriving (Eq)

instance Show Size where
    show L = "l"
    show Q = "q"

data Asm = Mov Size Operand Operand
         | Push Size Operand
         | Pop Size Operand
         | Global String
         | Type String ObjType
         | Label String
         | SText
         | SFile String
         | Ret
           deriving (Eq)

ind :: String
ind = "    "

instance Show Asm where
    show Ret             = ind ++ "ret"
    show (Mov s op1 op2) = ind ++ "mov" ++ (show s) ++ "\t" ++ show op1 ++ ", " ++ show op2
    show (Global s)      = ind ++ ".globl\t" ++ s
    show (Label s)       = s ++ ":"
    show (Pop s op)      = ind ++ "pop" ++ (show s) ++ "\t" ++ (show op)
    show (Push s op)     = ind ++ "push" ++ (show s) ++ "\t" ++ (show op)
    show SText           = ind ++ ".text"
    show (SFile file)    = ind ++ ".file\t\"" ++ file ++ "\""
    show (Type s ty)     = ind ++ ".type\t" ++ s ++ ", " ++ (show ty)

toAssembly :: [Asm] -> String
toAssembly = unlines . (map show)