{-# LANGUAGE OverloadedStrings #-}
module AST
    ( Unit (..), reduce
    , Section (..), reduceExtDecl
    , Formattable (..)
    , Name (..)
    , Function (..), reduceFuncDef
    , Statement (..)
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


data Local = Local TypeSpec Name
             deriving (Eq, Show)

instance Formattable TypeSpec where
  format TInt   = "int"
  format TVoid  = "void"

instance Formattable Local where
  format (Local ty n) = format n ++ " :: " ++ format ty ++ " "


data Parameter = Param TypeSpec (Maybe Name)
                 deriving (Eq, Show)

instance Formattable Parameter where
  format (Param t (Just n)) = format t ++ " " ++ format n
  format (Param t Nothing)  = format t

data Unit = Unit [ Section ]
            deriving (Eq, Show)

data Section = SectionFunc Function
               deriving (Eq, Show)

data Function = Function TypeSpec Name [Parameter] [Local] [Statement]
                deriving (Eq, Show)

instance Formattable Function where
  format (Function ty n p l _) = typ ++ " " ++ name ++ params ++ "\n" ++ locals
    where typ    = format ty
          name   = format n
          params = "(" ++ (intercalate ", " $ map format p) ++ ")"
          locals = "-- locals --\n" ++ (intercalate "\n" $ map format l)


data Statement = Return (Maybe Name)
               | Goto Name
               | Continue
               | Break
                 deriving (Eq, Show)

------------------------------------------------------------------------
-- Reducers ------------------------------------------------------------
------------------------------------------------------------------------

reduce :: P.TranslationUnit -> Unit
reduce (P.TranslationUnit decls) = Unit $ map reduceExtDecl decls

reduceExtDecl :: P.ExternalDecl -> Section
reduceExtDecl (P.ExtDeclFuncDef fn) = SectionFunc (reduceFuncDef fn)

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

reduceJump :: P.Jump -> Statement
reduceJump (P.Goto ident) = Goto $ reduceIdentifier ident
reduceJump (P.Continue) = Continue
reduceJump (P.Break) = Break
reduceJump (P.Return Nothing) = Return Nothing
