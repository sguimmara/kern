{-# LANGUAGE OverloadedStrings #-}
module AST
    ( AST (..), reduce, emptyAst
    , TopLevelElement (..), reduceExtDecl
    , Formattable (..)
    , Name (..)
    , Literal (..)
    , Function (..), reduceFuncDef
    , Statement (..), reduceStmt, reduceExprStmt
    , Expression (..), reduceExpr, reduceAssignExpr
    , Parameter (..)
    , Local (..)
    ) where

import           Core
import qualified Parser as P
import           Data.List     (intercalate)
import           Data.Text     (Text, pack, unpack)

class Formattable a where
  format :: a -> String

newtype Name = Name Text
               deriving (Eq, Show)

instance Formattable Name where
  format (Name x) = unpack x

data Literal = IntL Int
               deriving (Eq)

instance Show Literal where
  show (IntL n) = show n ++ "i"

data Local = Local TypeSpec Name
             deriving (Eq, Show)

instance Formattable TypeSpec where
  format IntT   = "int"
  format VoidT  = "void"

instance Formattable Local where
  format (Local ty n) = format n ++ " :: " ++ format ty ++ " "


data Parameter = Param TypeSpec (Maybe Name)
                 deriving (Eq, Show)

instance Formattable Parameter where
  format (Param t (Just n)) = format t ++ " " ++ format n
  format (Param t Nothing)  = format t

data AST = AST [ TopLevelElement ]
            deriving (Eq, Show)

emptyAst = AST []

data TopLevelElement = Func Function
                       deriving (Eq, Show)

data Function = Function TypeSpec Name [Parameter] [Local] [Statement]
                deriving (Eq, Show)

instance Formattable Function where
  format (Function ty n p l _) = typ ++ " " ++ name ++ params ++ "\n" ++ locals
    where typ    = format ty
          name   = format n
          params = "(" ++ (intercalate ", " $ map format p) ++ ")"
          locals = "-- locals --\n" ++ (intercalate "\n" $ map format l)


data Expression = Add Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | AssignEq Expression Expression
                | Var Name
                | Constant Literal
                  deriving (Eq)

instance Show Expression where
  show (Var (Name n)) = unpack n
  show (Constant l)   = show l
  show (Add a b)      = (show a) ++ " + " ++ (show b)
  show (Mul a b)      = (show a) ++ " * " ++ (show b)
  show (Div a b)      = (show a) ++ " / " ++ (show b)
  show (AssignEq l r) = (show l) ++ " := " ++ (show r)

data Statement = Return (Maybe Name)
               | Goto Name
               | Continue
               | Nop
               | ExprStmt Expression
               | Break
                 deriving (Eq)

instance Show Statement where
  show Break = "break;"
  show Continue = "continue;"
  show (Goto name) = "goto " ++ (show name) ++ ";"
  show (Return n) = "return;"
  show (ExprStmt e) = show e

------------------------------------------------------------------------
-- Reducers ------------------------------------------------------------
------------------------------------------------------------------------

reduce :: P.TranslationUnit -> AST
reduce (P.TranslationUnit decls) = AST $ map reduceExtDecl decls

reduceExtDecl :: P.ExternalDecl -> TopLevelElement
reduceExtDecl (P.ExtDeclFuncDef fn) = Func (reduceFuncDef fn)

reduceFuncDef :: P.FuncDef -> Function
reduceFuncDef (P.FuncDef declspec declar params stmts) =
  Function t n p l s
    where t = reduceDeclSpec declspec
          n = reduceDeclarator declar
          p = reduceParamList params
          l = []
          s = reduceCompoundStmt stmts

reduceDeclSpec :: P.DeclSpec -> TypeSpec
reduceDeclSpec (P.DeclTypeSpec ts) = ts

reduceDeclarator :: P.Declarator -> Name
reduceDeclarator (P.Decl (P.DeclIdent ident)) = reduceIdentifier ident

reduceIdentifier :: P.Identifier -> Name
reduceIdentifier (P.Identifier t) = Name t

reduceParamList :: P.ParamList -> [Parameter]
reduceParamList (P.ParamList pdecls) = map reduceParamDecl pdecls

reduceParamDecl :: P.ParamDecl -> Parameter
reduceParamDecl (P.ParamDecl declspec declar) = Param t (Just n)
  where t = reduceDeclSpec declspec
        n = reduceDeclarator declar

reduceCompoundStmt :: P.CompoundStmt -> [Statement]
reduceCompoundStmt (P.CompoundStmt stmts) = map reduceStmt stmts

reduceStmt :: P.Statement -> Statement
reduceStmt (P.JumpStmt jump) = reduceJump jump
reduceStmt (P.ExprStmt exprstmt) = reduceExprStmt exprstmt

reduceJump :: P.Jump -> Statement
reduceJump (P.Goto ident) = Goto $ reduceIdentifier ident
reduceJump (P.Continue) = Continue
reduceJump (P.Break) = Break
reduceJump (P.Return Nothing) = Return Nothing

reduceExprStmt :: P.ExpressionStmt -> Statement
reduceExprStmt (P.ExpressionStmt Nothing) = Nop
reduceExprStmt (P.ExpressionStmt (Just expr)) = ExprStmt $ reduceExpr expr

reduceExpr :: P.Expression -> Expression
reduceExpr (P.ExprAssignExpr ae) = reduceAssignExpr ae

reduceAssignExpr :: P.AssignExpr -> Expression
reduceAssignExpr (P.AssignExprUn unary (P.Assign) expr) =
  AssignEq (reduceUnaryExpr unary) (reduceAssignExpr expr)
reduceAssignExpr (P.AssignCond e) = reduceCondExpr e

reduceCondExpr :: P.CondExpr -> Expression
reduceCondExpr (P.CondExprLogOrExpr e) = reduceLogicalOrExpr e

reduceLogicalOrExpr :: P.LogicalOrExpr -> Expression
reduceLogicalOrExpr (P.LOEAnd e) = reduceLogicalAndExpr e

reduceLogicalAndExpr :: P.LogicalAndExpr -> Expression
reduceLogicalAndExpr (P.LAEInclusiveOr e) = reduceInclusiveOrExpr e

reduceInclusiveOrExpr :: P.InclusiveOrExpr -> Expression
reduceInclusiveOrExpr (P.IOEExclusiveOr e) = reduceExclusiveOrExpr e

reduceExclusiveOrExpr :: P.ExclusiveOrExpr -> Expression
reduceExclusiveOrExpr (P.EOEAndExpr e) = reduceAndExpr e

reduceAndExpr :: P.AndExpr -> Expression
reduceAndExpr (P.AndExprEqual e) = reduceEqualExpr e

reduceEqualExpr :: P.EqualExpr -> Expression
reduceEqualExpr (P.EqualExprRelat e) = reduceRelatExpr e

reduceRelatExpr :: P.RelatExpr -> Expression
reduceRelatExpr (P.RelatShift e) = reduceShiftExpr e

reduceShiftExpr :: P.ShiftExpr -> Expression
reduceShiftExpr (P.ShiftExprAdd e) = reduceAdditiveExpr e

reduceAdditiveExpr :: P.AdditiveExpr -> Expression
reduceAdditiveExpr (P.AdditiveExprMul e) = reduceMultiplicativeExpr e

reduceMultiplicativeExpr :: P.MultiplicativeExpr -> Expression
reduceMultiplicativeExpr (P.MultiExprCast e) = reduceCastExpr e

reduceCastExpr :: P.CastExpr -> Expression
reduceCastExpr (P.CastExprUn e) = reduceUnaryExpr e

reduceUnaryExpr :: P.UnaryExpr -> Expression
reduceUnaryExpr (P.UnaryExprPF (P.PFPrimeExpr (P.IdentExpr ident))) = Var $ reduceIdentifier ident
reduceUnaryExpr (P.UnaryExprPF (P.PFPrimeExpr (P.ConstExpr const))) = reduceConstant const

reduceConstant :: P.Constant -> Expression
reduceConstant (P.IntConst n) = Constant (IntL n)