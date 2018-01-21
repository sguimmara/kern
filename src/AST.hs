{-# LANGUAGE OverloadedStrings #-}

module AST
    ( Variable (..)
    , variable
    , Literal (..)
    , literal
    , Statement (..)
    , statement
    , Return (..)
    , returnstmt
    , Type
    , ctype
    , Parameter
    , parameter
    , ParamList
    , paramlist
    , Function (..)
    , function
    , FuncBody
    , body
    , TranslationUnit (..)
    , translunit
    ) where

import Data.Text        (Text, unpack, pack)
import Text.Parsec
import Text.Parsec.Text

------------------------------------------------------------------------
-- Utilities -----------------------------------------------------------
------------------------------------------------------------------------

digits :: String
digits = ['0' .. '9']

int :: GenParser st Int
int = do
    x <- many1 digit
    return $ (read x :: Int)
lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']

uppercaseLetters :: String
uppercaseLetters = ['A' .. 'Z']

letters :: String
letters = lowercaseLetters ++ uppercaseLetters

identifier :: GenParser st Text
identifier = do
    spaces
    let initChars = '_' : letters
    x <- oneOf initChars
    xs <- many $ oneOf (digits ++ initChars)
    return $ (pack (x : xs))

------------------------------------------------------------------------
-- AST definitions -----------------------------------------------------
------------------------------------------------------------------------

newtype Variable = Var Text
                   deriving (Show, Eq)

variable :: GenParser st Variable
variable = Var <$> identifier


data Literal = IntLit Int
               deriving (Show, Eq)

literal :: GenParser st Literal
literal = IntLit <$> int


data Return = ReturnVar Variable
            | ReturnLit Literal
              deriving (Show, Eq)

returnstmt :: GenParser st Return
returnstmt = do
    _ <- string "return"
    spaces
    ReturnVar <$> variable <|> ReturnLit <$> literal


data Statement = ReturnStmt Return
                 deriving (Show, Eq)

statement :: GenParser st Statement
statement = do
    stmt <- ReturnStmt <$> returnstmt
    spaces
    _ <- char ';'
    return stmt

data Type = TyInt
          | TyVoid
            deriving (Show, Eq)

ctype :: GenParser st Type
ctype = do
    (string "int" >> return TyInt) <|> (string "void" >> return TyVoid)

type ParamList = [Parameter]

data Parameter = Param Type Text
                 deriving (Show, Eq)

parameter :: GenParser st Parameter
parameter = Param <$> ctype <*> identifier

paramlist :: GenParser st ParamList
paramlist = between (spaces >> char '(') (spaces >> char ')') (many parameter)

type FuncBody = [Statement]

body :: GenParser st FuncBody
body = between (spaces >> char '{') (spaces >> char '}') (spaces >> many1 statement)

data Function = Func Type Text ParamList FuncBody
                deriving (Show, Eq)

function :: GenParser st Function
function = spaces >> Func <$> ctype <*> identifier <*> paramlist <*> body


data TranslationUnit = TranslationUnit String [Function]
                       deriving (Show, Eq)

translunit :: String -> GenParser st TranslationUnit
translunit s = TranslationUnit  s <$> many function