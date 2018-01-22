{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    , digits, int
    , Identifier (..), identifier
    , TranslationUnit (..), translationunit
    , ExternalDecl (..), externaldecl
    , FuncDef (..), funcdef
    , Declarator (..), declarator
    , ParamList (..), paramlist
    , ParamDecl (..), paramdecl
    , DeclSpec (..), declspec
    , TypeSpec (..), typespec
    , PrimExpr (..), primexpr
    , Constant (..), constant
    , Statement (..), statement
    , CompoundStmt(..), compoundstmt
    , Jump (..), jump
    ) where

import           Data.Text           (Text, unpack, pack)
import           Text.Parsec         ( spaces, digit
                                     , optionMaybe
                                     , string, char
                                     , sepBy, between
                                     , oneOf, many, many1
                                     , choice)
import qualified Text.Parsec as P
import           Text.Parsec.Text    (GenParser)
import           Text.Parsec.Error   (ParseError)

parse :: Text -> FilePath -> Either ParseError TranslationUnit
parse txt path = P.parse translationunit "" txt

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

------------------------------------------------------------------------
-- Grammar -------------------------------------------------------------
------------------------------------------------------------------------

-- TranslationUnit -----------------------------------------------------
data TranslationUnit = TranslationUnit  [ExternalDecl]
                       deriving (Eq, Show)

translationunit :: GenParser st TranslationUnit
translationunit = TranslationUnit <$> many externaldecl


data ExternalDecl = ExtDeclFuncDef FuncDef
                    deriving (Eq, Show)

externaldecl :: GenParser st ExternalDecl
externaldecl = spaces >> choice [ ExtDeclFuncDef <$> funcdef ]

-- Function ------------------------------------------------------------
data FuncDef = FuncDef DeclSpec Declarator ParamList CompoundStmt
               deriving (Eq, Show)

funcdef :: GenParser st FuncDef
funcdef = FuncDef <$> declspec <*> declarator <*> paramlist <*> compoundstmt

-- Declarator ----------------------------------------------------------
data Declarator = Decl DirectDecl
                  deriving (Eq, Show)

declarator :: GenParser st Declarator
declarator = Decl <$> directdecl

data DirectDecl = DeclIdent Identifier
                | DirectDeclP DirectDecl ParamList
                  deriving (Eq, Show)

directdecl :: GenParser st DirectDecl
directdecl = spaces >> choice [ DeclIdent <$> identifier ]


data ParamList = ParamList [ParamDecl]
                 deriving (Eq, Show)

comma = spaces >> char ',' >> spaces

paramlist :: GenParser st ParamList
paramlist = do
  spaces >> char '(' >> spaces
  p <- ParamList <$> (paramdecl `sepBy` comma)
  spaces >> char ')' >> spaces
  return p


data ParamDecl = ParamDecl DeclSpec Declarator
                 deriving (Eq, Show)

paramdecl :: GenParser st ParamDecl
paramdecl = choice [ ParamDecl <$> declspec <*> declarator ]


data DeclSpec = DeclTypeSpec TypeSpec
                deriving (Eq, Show)

declspec :: GenParser st DeclSpec
declspec = choice [ DeclTypeSpec <$> typespec ]


data TypeSpec = TVoid
              | TInt
                deriving (Eq, Show)

typespec :: GenParser st TypeSpec
typespec = choice [ string "void" >> return TVoid
                  , string "int" >> return TInt ]

-- Identifiers ---------------------------------------------------------
newtype Identifier = Identifier Text
                     deriving (Eq, Show)

identifier :: GenParser st Identifier
identifier = do
    spaces
    let initChars = '_' : letters
    x <- oneOf initChars
    xs <- many $ oneOf (digits ++ initChars)
    return $ Identifier (pack (x : xs))

-- Expressions ---------------------------------------------------------
data PrimExpr = ConstExpr Constant
                deriving (Eq, Show)

primexpr :: GenParser st PrimExpr
primexpr = choice [ ConstExpr <$> constant ]

data Constant = IntConst Int
              -- | CharConst Char
              -- | FloatConst Float
              -- | EnumConst
                deriving (Eq, Show)

constant :: GenParser st Constant
constant = choice [ IntConst <$> int ]

-- Statements ----------------------------------------------------------
data Statement = JumpStmt Jump
               | Compound CompoundStmt
                 deriving (Eq, Show)

statement :: GenParser st Statement
statement = choice [ JumpStmt <$> jump ]


data CompoundStmt = CompoundStmt [Statement]
                    deriving (Eq, Show)

compoundstmt :: GenParser st CompoundStmt
compoundstmt = between (spaces >> char '{' >> spaces)
                       (spaces >> char '}' >> spaces)
                       (CompoundStmt <$> many statement)

data Jump = Goto Identifier
          | Continue
          | Break
          | Return (Maybe PrimExpr)
            deriving (Eq, Show)

jump :: GenParser st Jump
jump = do
  jmp <- choice [ string "goto" >> spaces >> Goto <$> identifier
                , string "continue" >> return Continue
                , string "break" >> return Break
                , string "return" >> spaces >> Return <$> (optionMaybe primexpr)
                ]
  spaces >> char ';' >> spaces
  return jmp
