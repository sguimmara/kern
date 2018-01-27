{-# LANGUAGE OverloadedStrings #-}
module AST
    ( AST (..), reduce
    , TopLevelElement (..), reduceExtDecl
    , Name (..)
    , Literal (..)
    , Function (..), reduceFuncDef
    , Statement (..), reduceStmt, reduceExprStmt
    , Expression (..), reduceExpr, reduceAssignExpr, var, intL, add
    , Parameter (..)
    , Local (..)
    ) where

import           Core
import qualified Parser as P
import           Data.Yaml
import           Data.List           (intercalate)
import           Data.Text           (Text, pack, unpack)
import           Text.Parsec.Error   (ParseError)

newtype Name = Name Text
               deriving (Eq, Show)

data Literal = IntL Int
               deriving (Eq, Show)

data Local = Local TypeSpecifier Name
             deriving (Eq, Show)

data Parameter = Param TypeSpecifier (Maybe Name)
                 deriving (Eq, Show)

data AST = AST [ TopLevelElement ]
            deriving (Eq, Show)

data TopLevelElement = Func Function
                       deriving (Eq, Show)

data Function = Function TypeSpecifier Name [Parameter] [Local] [Statement]
                deriving (Eq, Show)

data Expression = Add Expression Expression
                | Mul Expression Expression
                | Div Expression Expression
                | AssignEq Expression Expression
                | Var Name
                | Constant Literal
                  deriving (Eq, Show)

add :: Expression -> Expression -> Expression
add lhs rhs = Add lhs rhs

var :: Text -> Expression
var t = Var (Name t)

intL :: Int -> Expression
intL n = Constant (IntL n)

data Statement = Return (Maybe Name)
               | Goto Name
               | Continue
               | Nop
               | ExprStmt Expression
               | Break
                 deriving (Eq, Show)

------------------------------------------------------------------------
-- YAML representations ------------------------------------------------
------------------------------------------------------------------------

instance ToJSON Name where
  toJSON (Name t) = String t

instance ToJSON Literal where
  toJSON (IntL n) = object [("int-literal", toJSON n)]

instance ToJSON Local where
  toJSON (Local ty n) = object [ ("name", toJSON n)
                               , ("type", toJSON ty) ]

instance ToJSON Parameter where
  toJSON (Param ty Nothing)  = object [ ("type", toJSON ty) ]
  toJSON (Param ty (Just n)) = object [ ("type", toJSON ty)
                                      , ("name", toJSON n) ]

instance ToJSON Expression where
  toJSON (Var n)          = object [ ("var", toJSON n) ]
  toJSON (Constant c)     = object [ ("const", toJSON c) ]
  toJSON (Add e0 e1)      = object [ ("add", array [ toJSON e0, toJSON e1 ]) ]
  toJSON (Mul e0 e1)      = object [ ("mul", array [ toJSON e0, toJSON e1 ]) ]
  toJSON (Div e0 e1)      = object [ ("mul", array [ toJSON e0, toJSON e1 ]) ]
  toJSON (AssignEq e0 e1) = object [ ("assign", array [ toJSON e0, toJSON e1 ]) ]

instance ToJSON Statement where
  toJSON (Return Nothing)  = String "return"
  toJSON (Return (Just n)) = object [ ("return", toJSON n) ]
  toJSON (Goto n)          = object [ ("goto", toJSON n) ]
  toJSON Continue          = String "continue"
  toJSON Nop               = String "no-op"
  toJSON Break             = String "break"
  toJSON (ExprStmt expr)   = object [ ("expr-statement", toJSON expr) ]

instance ToJSON Function where
  toJSON (Function ty n ps ls ss) =
    object [ ("type", toJSON ty)
           , ("name", toJSON n)
           , ("local-variables", array $ map toJSON ls)
           , ("parameters", array $ map toJSON ps)
           , ("statements", array $ map toJSON ss)
           ]

instance ToJSON TopLevelElement where
  toJSON (Func f) = object [ ("function", toJSON f) ]

instance ToJSON AST where
  toJSON (AST xs) = object [ ("AST", array $ map toJSON xs) ]

------------------------------------------------------------------------
-- Reducers ------------------------------------------------------------
------------------------------------------------------------------------

{-- Reducers are functions that map from parse tree nodes to AST nodes.
    They effectively reduce the complexity of the tree by removing non
    essential cruft that only serves in the parsing step, to ensure
    the syntax is correct.
--}

reduce :: P.TranslationUnit -> Either ParseError AST
reduce (P.TranslationUnit decls) = Right $ AST $ map reduceExtDecl decls

reduceExtDecl :: P.ExternalDeclaration -> TopLevelElement
reduceExtDecl (P.ExtDeclFunc fn) = Func (reduceFuncDef fn)

reduceFuncDef :: P.FunctionDefinition -> Function
reduceFuncDef (P.FunctionDefinition declspecs declar params stmts) =
  Function t n p l s
    where t = reduceDeclarationSpecifier $ head declspecs --FIXME
          n = reduceDeclarator declar
          p = [] --FIXME
          l = []
          s = reduceCompoundStatement stmts

reduceDeclarationSpecifier :: P.DeclarationSpecifier -> TypeSpecifier
reduceDeclarationSpecifier (P.DeclSpecSpec ts) = ts

reduceDeclarator :: P.Declarator -> Name
reduceDeclarator (P.Declarator p (P.DirectDeclId ident)) = reduceIdentifier ident --FIXME

reduceIdentifier :: P.Identifier -> Name
reduceIdentifier (P.Identifier t) = Name t

reduceParamList :: P.ParamList -> [Parameter]
reduceParamList (P.ParamList pdecls) = map reduceParameterDeclaration pdecls

reduceParameterDeclaration :: P.ParameterDeclaration -> Parameter
reduceParameterDeclaration (P.ParameterDeclaration declspec declar) = Param t (Just n)
  where t = head $ map reduceDeclarationSpecifier declspec --FIXME
        n = reduceDeclarator declar

reduceCompoundStatement :: P.CompoundStatement -> [Statement]
reduceCompoundStatement (P.CompoundStatement decl stmts) = map reduceStmt stmts --FIXME

reduceStmt :: P.Statement -> Statement
reduceStmt (P.JumpStmt jump) = reduceJump jump
reduceStmt (P.ExprStmt exprstmt) = reduceExprStmt exprstmt

reduceJump :: P.Jump -> Statement
reduceJump (P.Goto ident)      = Goto $ reduceIdentifier ident
reduceJump (P.Continue)        = Continue
reduceJump (P.Break)           = Break
reduceJump (P.Return Nothing)  = Return Nothing

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