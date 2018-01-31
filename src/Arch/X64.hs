module Arch.X64
    ( Generable (..)
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
