module AST
    ( Formattable (..)
    , Name (..)
    , Function (..)
    , Parameter (..)
    , Local (..)
    ) where

import Parser
import Data.List   (intercalate)

class Formattable a where
  format :: a -> String

newtype Name = Name String
               deriving (Eq, Show)

instance Formattable Name where
  format (Name x) = x

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

data Function = Function TypeSpec Name [Parameter] [Local] [Statement]
                deriving (Eq, Show)

instance Formattable Function where
  format (Function ty n p l _) = typ ++ " " ++ name ++ params ++ "\n" ++ locals
    where typ    = format ty
          name   = format n
          params = "(" ++ (intercalate ", " $ map format p) ++ ")"
          locals = "-- locals --\n" ++ (intercalate "\n" $ map format l)
