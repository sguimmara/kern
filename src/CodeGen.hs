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
import Data.Char    (toLower)
import Data.List    (intercalate)


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

data Asm = Label String
         -- Sections ---------------------------------------------------
         | SText
         | SFile String
         | Global String
         | Type String ObjType
         -- Instructions------------------------------------------------
         | Ret
         | Mov Size Operand Operand
         | Pop Size Operand
         | Push Size Operand
           deriving (Eq)

ind = "\t"

tab = "\t"
fmt []     = ""
fmt [x] = tab ++ x
fmt (x:xs) = tab ++ x ++ fmt xs

lst :: [String] -> String
lst = intercalate ", "

sized :: String -> Size -> String
sized x s = x ++ show s

sh x = show x

instance Show Asm where
    -- Misc ------------------------------------------------------------
    show (Label s)          = s ++ ":"
    -- Sections --------------------------------------------------------
    show SText              = fmt [ ".text" ]
    show (SFile file)       = fmt [ ".file", file ]
    show (Type s ty)        = fmt [ ".type", lst [s, sh ty] ]
    show (Global s)         = fmt [ ".globl", s ]
    -- Instructions-----------------------------------------------------
    show Ret                = fmt [ "ret" ]
    show (Mov s op1 op2)    = fmt [ sized "mov" s, lst [sh op1, sh op2] ]
    show (Pop s op)         = fmt [ sized "pop" s, sh op ]
    show (Push s op)        = fmt [ sized "push" s, sh op ]

toAssembly :: [Asm] -> String
toAssembly = unlines . (map show)