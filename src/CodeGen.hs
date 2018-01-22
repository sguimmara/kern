module CodeGen
    ( genAssembly
    , Register (..)
    , Operand (..)
    , Asm (..)
    , Size (..)
    , SymbolTable (..)
    , SymMap (..)
    , mkTable
    , mkParamMap
    , mkBodyMap
    , lookupTbl
    , loc
    , genFunc
    , genStmt
    , genRet
    ) where

import AST

import Data.Text    (pack, unpack)
import Data.Char    (toLower)
import Data.List    (intercalate)

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
--- Symbol table -------------------------------------------------------
------------------------------------------------------------------------

data SymbolTable = SymTable [SymMap]
                   deriving (Eq)

instance Show SymbolTable where
  show (SymTable syms) = unlines $ map show syms

data SymMap = SymMap SymType Identifier TypeSpec Location
              deriving (Eq)

loc :: SymMap -> Location
loc (SymMap _ _ _ l) = l

instance Show SymMap where
  show (SymMap symty ident ty loc) =
    intercalate " | " [ show ident
                      , show symty
                      ,  show ty
                      , show loc ]

type Offset = Int

data SymType = ParamSym | LocalSym
               deriving (Eq)

instance Show SymType where
    show ParamSym = "param"
    show LocalSym = "local"

data Location = LocReg Register
              | LocStack Offset
                deriving (Eq)

instance Show Location where
    show (LocReg r) = map toLower (show r)
    show (LocStack offs) = show offs

-- Make a symbol table following the SystemV AMD64 ABI
-- That is :
-- The 6 first integer parameters are stored in rdi, rsi, rdx, rcx, r8, r9
mkTable :: Function -> SymbolTable
mkTable (Func _ _ params body) = SymTable $ mkParamMap params ++ mkBodyMap body

-- create maps from the given function parameters
mkParamMap :: ParamList -> [SymMap]
mkParamMap lst = f lst 0
    where f [Param (Var ty ident)] i    = [SymMap ParamSym ident ty (getLoc ty i)]
          f (Param (Var ty ident):xs) i = (SymMap ParamSym ident ty (getLoc ty i) : f xs (i+1))

getLoc :: TypeSpec -> Int -> Location
getLoc IntS i = case i of
                  0 -> LocReg Rdi
                  1 -> LocReg Rsi
                  2 -> LocReg Rdx
                  3 -> LocReg Rcx
                  4 -> LocReg R8
                  5 -> LocReg R9

lookupTbl :: SymbolTable -> Identifier -> SymMap
lookupTbl (SymTable syms) i = lk syms i
  where lk ( r@(SymMap _ x _ _ ) : xs) ii = if ii == x then r else lk xs ii

mkBodyMap :: FuncBody -> [SymMap]
mkBodyMap body = [] -- TODO : collect local variables

------------------------------------------------------------------------
--- Assembly -----------------------------------------------------------
------------------------------------------------------------------------

data Asm =
         -- Misc -------------------------------------------------------
           Label String
         | Section String [String]
         -- Instructions------------------------------------------------
         | Ret
         | Rep Asm
         | Mov Size Operand Operand
         | Pop Size Operand
         | Push Size Operand
           deriving (Eq, Show)

------------------------------------------------------------------------
--- Generators ---------------------------------------------------------
------------------------------------------------------------------------

genAssembly :: TranslationUnit -> [Asm]
genAssembly t@(TranslationUnit _ funcs) = genTUMeta t ++ concatMap genFunc funcs

genTUMeta :: TranslationUnit -> [Asm]
genTUMeta (TranslationUnit n _) = [ s_file (show n) ]

genFunc :: Function -> [Asm]
genFunc f = concat [ meta, prologue, body ]
  where table    = mkTable f
        meta     = genFuncMeta f
        prologue = genPrologue table f
        body     = genBody table (getBody f)

genFuncMeta :: Function -> [Asm]
genFuncMeta (Func _ name _ _) =
  let n = getId name in
        [ s_global n
        , s_type n TyFunction
        , Label n
        ]

genPrologue :: SymbolTable -> Function -> [Asm]
genPrologue st f = [ pushq rbp
                  , movq rsp rbp]

genBody :: SymbolTable -> FuncBody -> [Asm]
genBody st body = concatMap (genStmt st) body

genStmt :: SymbolTable -> Statement -> [Asm]
genStmt st (ReturnStmt ret) = genRet st ret

genRet :: SymbolTable -> Return -> [Asm]
genRet _ ReturnVoid       = [ Rep Ret ]
genRet _ (ReturnLit lit)  = [ movq rbp rsp
                            , movl (genLit lit) eax
                            , popq rbp
                            , Ret ]

genLit :: Literal -> Operand
genLit (IntLit x) = OpValue x

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
