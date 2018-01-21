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

class Emittable a where
    emit :: a -> [Asm]


instance Emittable Return where
    emit (ReturnLit lit) = [ Movl (emitLiteral lit) eax
                           , Popq rbp
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
fbody (Func _ _ _ stmts) = [ Pushq rbp ] ++ concatMap emit stmts

instance Emittable TranslationUnit where
    emit (TranslationUnit file funcs) = [ SFile file
                                        , SText ] ++ concatMap emit funcs

emitLiteral (IntLit x) = OpValue x

data Register = Eax | Rbp
                deriving (Show, Eq)

data Operand = OpValue Int
             | OpReg Register
               deriving (Eq)

instance Show Operand where
    show (OpValue x) = "$" ++ show x
    show (OpReg reg) = "%" ++ case reg of
                                     Eax -> "eax"
                                     Rbp -> "rbp"

data ObjType = TyFunction
               deriving (Eq)

instance Show ObjType where
    show TyFunction = "@function"

data Asm = Movl Operand Operand
         | Pushq Operand
         | Popq Operand
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
    show (Movl op1 op2) = ind ++ "movl " ++ show op1 ++ ", " ++ show op2
    show Ret            = ind ++ "ret"
    show (Global s)     = ind ++ ".globl " ++ s
    show (Label s)      = s ++ ":"
    show (Popq op)      = ind ++ "popq\t" ++ (show op)
    show (Pushq op)      = ind ++ "pushq\t" ++ (show op)
    show SText          = ind ++ ".text"
    show (SFile file)   = ind ++ ".file \"" ++ file ++ "\""
    show (Type s ty)    = ind ++ ".type " ++ s ++ ", " ++ (show ty)

toAssembly :: [Asm] -> String
toAssembly = unlines . (map show)