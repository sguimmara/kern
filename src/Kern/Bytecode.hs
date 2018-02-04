{-# LANGUAGE OverloadedStrings #-}

module Kern.Bytecode where

import           Kern.Core
import           Kern.AST

import           Data.Word
import           Data.Maybe
import           Data.List
import           Data.Int
import           Data.Yaml
import           Data.Vector      (Vector)
import qualified Data.Vector as V
import           Data.Text        (Text, pack, toLower)

data Listing = Listing (Vector Routine) deriving (Eq, Show)

instance ToJSON Listing where
  toJSON (Listing rs) = object [ ("listing", toJSON rs) ]

data Local = Local DataType deriving (Eq, Show)

mkLocal :: LocalVariable -> Local
mkLocal (LocalVariable (InternalType dt _ _ _) _ _) = Local dt

instance ToJSON Local where
  toJSON (Local dt) = object [ ("type", toJSON dt) ]

data Routine =
  Routine { name :: Text
          , locals :: (Vector Local)
          , instrs :: (Vector Instr)
          } deriving (Eq, Show)

instance ToJSON Routine where
  toJSON r =
    object [ ("routine",
      object
        [ ("name", toJSON $ name r)
        , ("locals",       (array . V.toList . (V.map toJSON)) (locals r))
        , ("instructions", (array . V.toList . (V.map toJSON)) (instrs r))
        ])
    ]

data Instr
  = Add           -- Replace the 2 topmost values with their sum
  | Sub           -- Replace the 2 topmost values with their difference
  | Mul           -- Replace the 2 topmost values with their product
  | Div           -- Replace the 2 topmost values with their quotient
  | Ldc64 Int64   -- push signed 64bit word onto the stack
  | Ldc32 Int32   -- push signed 64bit word onto the stack
  | Ret64         -- exit from current routine, returning no value
  | Ret           -- exit from current routine, returning no value
  | Stloc Int     -- Pops top stack value into local variable N
  | Ldloc Int     -- Push local variable N onto the stack
  deriving (Eq, Show, Ord)

instance ToJSON Instr where
  toJSON i =
    case i of
      Stloc n   -> String $ pack ("stloc " ++ show n)
      Ldloc n   -> String $ pack ("ldloc " ++ show n)
      Ldc64 i64 -> String $ pack ("ldc64 " ++ show i64)
      Ldc32 i32 -> String $ pack ("ldc32 " ++ show i32)
      x         -> String $ toLower $ pack (show x)

data Context
  = FuncContext FunctionDefinition
  | NullContext
  deriving (Eq, Show)

getLocIdx :: Identifier -> Context -> Int
getLocIdx i0 (FuncContext fn) =
  let locs = (bodyLocals . funcBody) fn
      idx = findIndex (\(LocalVariable _ i1 _) -> i0 == i1) locs in
  case idx of
    Just i  -> i
    Nothing -> error "could not locate local variable"

class Emittable a where
  emit :: Context -> a -> Vector Instr

genTrUnit :: TranslationUnit -> Listing
genTrUnit (TranslationUnit decls) = Listing (V.fromList rs)
  where rs  = catMaybes $ map gen decls
        gen (FunctionDefinition f) = Just (genFun f)
        gen _                      = Nothing


genFun :: FunctionDefinition -> Routine
genFun fn = Routine n ls ins
  where (Ident n) = funcName fn
        ctx       = FuncContext fn
        ls        = V.fromList $ map mkLocal ((bodyLocals . funcBody) fn)
        ins       = V.concat $ map (emit ctx) ((bodyStmts . funcBody) fn)

instance Emittable Statement where
  emit ctx (Jump j)            = emit ctx j
  emit ctx (ExprStmt Nothing)  = V.empty
  emit ctx (ExprStmt (Just e)) = emit ctx e

instance Emittable Jump where
  emit ctx (Return Nothing)  = V.singleton Ret
  emit ctx (Return (Just e)) = V.snoc (emit ctx e) Ret

instance Emittable Expr where
  emit ctx (Assign lhs op rhs)  =
    V.concat [emit ctx rhs, epi]
      where epi = case lhs of
                    (PrimI i) -> V.singleton (Stloc $ getLocIdx i ctx)
                    _         -> error "unsupported construct"

  emit ctx (MulExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Mul
  emit ctx (DivExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Div
  emit ctx (AddExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Add
  emit ctx (SubExpr e0 e1)      =
    V.snoc (V.concat [emit ctx e0, emit ctx e1]) Sub
  emit ctx (PrimC (Int64Lit n)) = V.fromList [Ldc64 n]
  emit ctx (PrimC (Int32Lit n)) = V.fromList [Ldc32 n]
  emit ctx (PrimI ident)        = V.fromList [Ldloc n]
                                  where n = getLocIdx ident ctx

