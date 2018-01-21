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

class Emittable a where
    emit :: a -> [Asm]

instance Emittable Return where
    emit (ReturnLit lit) = [ Movl (emitLiteral lit) (OpRegister Eax)
                           , Ret
                           ]

instance Emittable Statement where
    emit (ReturnStmt ret) = emit ret

instance Emittable Function where
    emit (Func _ name _ stmts) = let asmname = unpack name in
                                 [ Global asmname
                                 , Type asmname TyFunction
                                 , Label asmname
                                 ] ++ (concatMap emit stmts)

instance Emittable TranslationUnit where
    emit (TranslationUnit funcs) = concatMap emit funcs

emitLiteral (IntLit x) = OpValue x

data Register = Eax
                deriving (Show, Eq)

data Operand = OpValue Int
             | OpRegister Register
               deriving (Eq)

instance Show Operand where
    show (OpValue x) = "$" ++ show x
    show (OpRegister reg) = "%" ++ case reg of
                                     Eax -> "eax"

data ObjType = TyFunction
               deriving (Eq)

instance Show ObjType where
    show TyFunction = "@function"

data Asm = Movl Operand Operand
         | Global String
         | Type String ObjType
         | Label String
         | Ret
           deriving (Eq)

ind :: String
ind = "    "

instance Show Asm where
    show (Movl op1 op2) = ind ++ "movl " ++ show op1 ++ ", " ++ show op2
    show Ret            = ind ++ "ret"
    show (Global s)     = ind ++ ".globl " ++ s
    show (Label s)      = s ++ ":"
    show (Type s ty)    = ind ++ ".type " ++ s ++ ", " ++ (show ty)

toAssembly :: [Asm] -> String
toAssembly = unlines . (map show)