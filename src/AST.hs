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

-- Type specifiers -----------------------------------------------------
data Type = TyInt
          | TyVoid
            deriving (Show, Eq)


-- Variables, literals and unary operators -----------------------------
newtype Variable = Var Text
                   deriving (Show, Eq)

data Literal = IntLit Int
               deriving (Show, Eq)


-- Statements ----------------------------------------------------------
data Statement = ReturnStmt Return
                 deriving (Show, Eq)

data Return = ReturnVar Variable
            | ReturnLit Literal
              deriving (Show, Eq)


-- Functions -----------------------------------------------------------
data Function = Func Type Text ParamList FuncBody
                deriving (Show, Eq)

type FuncBody = [Statement]

type ParamList = [Parameter]

data Parameter = Param Type Text
               deriving (Show, Eq)


-- Translation unit ----------------------------------------------------
data TranslationUnit = TranslationUnit
                         String         -- ^ The filename
                         [Function]     -- ^ The functions
                       deriving (Show, Eq)

------------------------------------------------------------------------
-- Parsing functions----------------------------------------------------
------------------------------------------------------------------------

-- Type specifiers -----------------------------------------------------
ctype :: GenParser st Type
ctype = do
    (string "int" >> return TyInt) <|> (string "void" >> return TyVoid)


-- Variables, literals and unary operators -----------------------------
variable :: GenParser st Variable
variable = Var <$> identifier

literal :: GenParser st Literal
literal = IntLit <$> int


-- Statements ----------------------------------------------------------
statement :: GenParser st Statement
statement = do
    stmt <- ReturnStmt <$> returnstmt
    spaces
    _ <- char ';'
    return stmt

returnstmt :: GenParser st Return
returnstmt = do
    _ <- string "return"
    spaces
    ReturnVar <$> variable <|> ReturnLit <$> literal


-- Functions -----------------------------------------------------------
parameter :: GenParser st Parameter
parameter = Param <$> ctype <*> identifier

paramlist :: GenParser st ParamList
paramlist = between (spaces >> char '(') (spaces >> char ')') (many parameter)

body :: GenParser st FuncBody
body = between (spaces >> char '{') (spaces >> char '}') (spaces >> many1 statement)

function :: GenParser st Function
function = spaces >> Func <$> ctype <*> identifier <*> paramlist <*> body


-- Translation unit ----------------------------------------------------
translunit :: String -> GenParser st TranslationUnit
translunit s = TranslationUnit  s <$> many function