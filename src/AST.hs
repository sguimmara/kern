{-# LANGUAGE OverloadedStrings #-}

module AST
    ( Variable (..)
    , variable
    , Identifier (..)
    , identifier
    , Literal (..)
    , literal
    , Statement (..)
    , statement
    , Return (..)
    , returnstmt
    , TypeSpec
    , typespec
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
-- AST definitions -----------------------------------------------------
------------------------------------------------------------------------

-- Type specifiers -----------------------------------------------------
data TypeSpec = IntS            -- ^ int
              | VoidS           -- ^ void
              | FloatS          -- ^ void
                deriving (Show, Eq)

-- Variables, literals and unary operators -----------------------------
data Variable = Var TypeSpec Identifier
                deriving (Show, Eq)

data Literal = IntLit Int
               deriving (Show, Eq)

newtype Identifier = Ident Text
                     deriving (Show, Eq)


-- Statements ----------------------------------------------------------
data Statement = ReturnStmt Return
                 deriving (Show, Eq)

data Return = ReturnVar Identifier
            | ReturnLit Literal
              deriving (Show, Eq)


-- Functions -----------------------------------------------------------
data Function = Func TypeSpec Identifier ParamList FuncBody
                deriving (Show, Eq)

type FuncBody = [Statement]

type ParamList = [Parameter]

newtype Parameter = Param Variable
                    deriving (Show, Eq)


-- Translation unit ----------------------------------------------------
data TranslationUnit = TranslationUnit
                         String         -- ^ The filename
                         [Function]     -- ^ The functions
                       deriving (Show, Eq)

------------------------------------------------------------------------
-- Parsing functions----------------------------------------------------
------------------------------------------------------------------------

-- Utilities -----------------------------------------------------------
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

identifier :: GenParser st Identifier
identifier = do
    spaces
    let initChars = '_' : letters
    x <- oneOf initChars
    xs <- many $ oneOf (digits ++ initChars)
    return $ Ident (pack (x : xs))


-- Type specifiers -----------------------------------------------------
typespec :: GenParser st TypeSpec
typespec = do
    let xs = map string ["int", "void", "float"]
    spec <- choice xs
    case spec of
        "void"  -> return VoidS
        "int"   -> return IntS
        "float" -> return FloatS

-- Variables, literals and unary operators -----------------------------
variable :: GenParser st Variable
variable = Var <$> typespec <*> identifier

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
    ReturnVar <$> identifier <|> ReturnLit <$> literal


-- Functions -----------------------------------------------------------
parameter :: GenParser st Parameter
parameter = Param <$> variable

paramlist :: GenParser st ParamList
paramlist = between (spaces >> char '(') (spaces >> char ')') params
    where params = sepBy parameter (spaces >> char ',' >> spaces)

body :: GenParser st FuncBody
body = between (spaces >> char '{') (spaces >> char '}') (spaces >> many1 statement)

function :: GenParser st Function
function = spaces >> Func <$> typespec <*> identifier <*> paramlist <*> body


-- Translation unit ----------------------------------------------------
translunit :: String -> GenParser st TranslationUnit
translunit s = TranslationUnit  s <$> many function