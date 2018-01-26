module CodeGen.Assembly
    ( Generable (..), generate
    , Register (..)
    , Operand (..)
    , Instr (..)
    ) where

import           Core
import           AST

import           Data.Text           (Text)

------------------------------------------------------------------------
--- Registers ----------------------------------------------------------
------------------------------------------------------------------------
data Register = -- General purpose -------------------------------------
                Eax | Ebx | Ecx | Edx | Ebp | Esi | Edi | Esp -- 32 bit
              | Rax | Rbx | Rcx | Rdx | Rbp | Rsi | Rdi | Rsp -- 64 bit
              | R8  | R9  | R10 | R11 | R12 | R13 | R14 | R15 -- 64 bit
                deriving (Show, Eq)

data Operand = OpValue Int
             | OpReg Register
               deriving (Eq)

------------------------------------------------------------------------
--- Instructions -------------------------------------------------------
------------------------------------------------------------------------
data Instr = Ret
           | Rep Instr
           | Label Text
             deriving (Eq, Show)

class Generable a where
  gen :: a -> [Instr]

instance Generable Statement where
  gen (Return Nothing)  = [ Ret ]
  gen (Return (Just n)) = [ Ret ]
  gen _ = []

instance Generable AST where
  gen (AST ast) = concatMap gen ast

generate :: AST -> [Instr]
generate ast = gen ast

instance Generable TopLevelElement where
  gen (Func fn) = gen fn

instance Generable Function where
  gen (Function _ (Name n) _ _ s) = [ Label n ] ++ (concatMap gen s)