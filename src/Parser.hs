{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse, parseForce
    , digits, int, float, decimal
    , Identifier (..), identifier
    , TranslationUnit (..), translationunit, emptyTranslationUnit
    , ExternalDecl (..), externaldecl
    , FuncDef (..), funcdef
    , Declarator (..), declarator
    , DirectDecl (..)
    , ParamList (..), paramlist
    , ParamDecl (..), paramdecl
    , DeclSpec (..), declspec
    , typespec
    , Expression (..), expr
    , CondExpr (..), condexpr
    , LogicalOrExpr (..), logicalOrExpr
    , LogicalAndExpr (..), logicalandexpr
    , InclusiveOrExpr (..), inclusiveOrExpr
    , ExclusiveOrExpr (..), exclusiveOrExpr
    , MultiplicativeExpr (..), multiplicativeExpr
    , AdditiveExpr (..), additiveExpr
    , ShiftExpr (..), shiftExpr
    , RelatExpr (..), relatExpr
    , CastExpr (..), castExpr
    , AndExpr (..), andexpr
    , EqualExpr (..), equalExpr
    , AssignExpr (..), assignexpr
    , AssignOp (..), assignop
    , PostfixExpr (..), postfixexpr
    , UnaryExpr (..), unaryexpr
    , PrimExpr (..), primexpr
    , Constant (..), constant
    , Statement (..), statement
    , ExpressionStmt (..), expressionStmt
    , CompoundStmt(..), compoundstmt
    , Jump (..), jump
    ) where

import           Core

import           Data.Either
import           Data.Text           (Text, unpack, pack)
import           Text.Parsec         ( spaces, digit, anyChar
                                     , optionMaybe
                                     , string, char
                                     , sepBy, between
                                     , oneOf, many, many1
                                     , choice, try, (<|>)
                                     , option, endBy)
import           Text.Parsec.Char    (satisfy)
import qualified Text.Parsec as P
import           Text.Parsec.Text    (GenParser)
import           Text.Parsec.Error   (ParseError)

parseForce :: Text -> TranslationUnit
parseForce t = fromRight (TranslationUnit []) (parse t)

parse :: Text -> Either ParseError TranslationUnit
parse txt = P.parse translationunit "" txt

------------------------------------------------------------------------
-- Utilities -----------------------------------------------------------
------------------------------------------------------------------------
digits :: String
digits = ['0' .. '9']

int :: GenParser st Int
int = do
  x <- many1 digit
  return $ (read x :: Int)

decimal :: GenParser st String
decimal = do
  s <- sequence [ many1 digit, string ".", many1 digit ]
  return $ concat s

double :: GenParser st Double
double = do
  d <- decimal
  return (read d :: Double)

float :: GenParser st Float
float = do
  d <- decimal
  char 'f'
  return (read d :: Float)

charLiteral :: GenParser st Char
charLiteral = between (char '\'') (char '\'') anyChar

lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']

uppercaseLetters :: String
uppercaseLetters = ['A' .. 'Z']

letters :: String
letters = lowercaseLetters ++ uppercaseLetters

lbracket = spaces >> char '{' >> spaces
rbracket = spaces >> char '}' >> spaces

lparen = spaces >> char '(' >> spaces
rparen = spaces >> char ')' >> spaces

------------------------------------------------------------------------
-- Grammar -------------------------------------------------------------
------------------------------------------------------------------------

-- TranslationUnit -----------------------------------------------------
data TranslationUnit = TranslationUnit  [ExternalDecl]
                       deriving (Eq, Show)

emptyTranslationUnit = TranslationUnit []

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


typespec :: GenParser st TypeSpec
typespec = choice [ string "void" >> return VoidT
                  , string "int" >> return IntT ]

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
data Expression = ExprAssignExpr AssignExpr
                  deriving (Eq, Show)

expr :: GenParser st Expression
expr = spaces >> choice [ ExprAssignExpr <$> assignexpr ]

data AssignExpr = AssignCond CondExpr
                | AssignExprUn UnaryExpr AssignOp AssignExpr
                  deriving (Eq, Show)

assignexpr :: GenParser st AssignExpr
assignexpr = spaces >>
             ((try (AssignExprUn <$> unaryexpr <*> assignop <*> assignexpr))
               <|> (AssignCond <$> condexpr))

data AssignOp = Assign
                deriving (Eq, Show)

assignop :: GenParser st AssignOp
assignop = spaces >> choice [ char '=' >> return Assign ]

data CondExpr = CondExprLogOrExpr LogicalOrExpr
                deriving (Eq, Show)

condexpr :: GenParser st CondExpr
condexpr = spaces >> choice [ CondExprLogOrExpr <$> logicalOrExpr ]

data LogicalOrExpr = LOEAnd LogicalAndExpr
                           deriving (Eq, Show)

logicalOrExpr :: GenParser st LogicalOrExpr
logicalOrExpr = spaces >> choice [ LOEAnd <$> logicalandexpr ]

data LogicalAndExpr = LAEInclusiveOr InclusiveOrExpr
                      deriving (Eq, Show)

logicalandexpr :: GenParser st LogicalAndExpr
logicalandexpr = spaces >> choice [ LAEInclusiveOr <$> inclusiveOrExpr ]

data InclusiveOrExpr = IOEExclusiveOr ExclusiveOrExpr
                       deriving (Eq, Show)

inclusiveOrExpr :: GenParser st InclusiveOrExpr
inclusiveOrExpr = spaces >> choice [ IOEExclusiveOr <$> exclusiveOrExpr ]

data ExclusiveOrExpr = EOEAndExpr AndExpr
                       deriving (Eq, Show)

exclusiveOrExpr :: GenParser st ExclusiveOrExpr
exclusiveOrExpr = spaces >> choice [ EOEAndExpr <$> andexpr ]

data AndExpr = AndExprEqual EqualExpr
               deriving (Eq, Show)

andexpr :: GenParser st AndExpr
andexpr = spaces >> choice [ AndExprEqual <$> equalExpr ]

data EqualExpr = EqualExprRelat RelatExpr
                 deriving (Eq, Show)

equalExpr :: GenParser st EqualExpr
equalExpr = spaces >> choice [ EqualExprRelat <$> relatExpr ]

data RelatExpr = RelatShift ShiftExpr
                 deriving (Eq, Show)

relatExpr :: GenParser st RelatExpr
relatExpr = spaces >> choice [ RelatShift <$> shiftExpr ]

data ShiftExpr = ShiftExprAdd AdditiveExpr
                 deriving (Eq, Show)

shiftExpr :: GenParser st ShiftExpr
shiftExpr = spaces >> choice [ ShiftExprAdd <$> additiveExpr ]

data AdditiveExpr = AdditiveExprMul MultiplicativeExpr
                    deriving (Eq, Show)

additiveExpr :: GenParser st AdditiveExpr
additiveExpr = spaces >> choice [ AdditiveExprMul <$> multiplicativeExpr ]

data MultiplicativeExpr = MultiExprCast CastExpr
                          deriving (Eq, Show)

multiplicativeExpr :: GenParser st MultiplicativeExpr
multiplicativeExpr = spaces >> choice [ MultiExprCast <$> castExpr ]

data CastExpr = CastExprUn UnaryExpr
                deriving (Eq, Show)

castExpr :: GenParser st CastExpr
castExpr = spaces >> choice [ CastExprUn <$> unaryexpr]

data UnaryExpr = UnaryExprPF PostfixExpr
                 deriving (Eq, Show)

unaryexpr :: GenParser st UnaryExpr
unaryexpr = choice [ UnaryExprPF <$> postfixexpr ]

data PostfixExpr = PFPrimeExpr PrimExpr
                   deriving (Eq, Show)

postfixexpr :: GenParser st PostfixExpr
postfixexpr = choice [ PFPrimeExpr <$> primexpr ]

data PrimExpr = ConstExpr Constant
              | IdentExpr Identifier
              | StringExpr Text
              | ParentExpr Expression
                deriving (Eq, Show)


primexpr :: GenParser st PrimExpr
primexpr = choice [ ConstExpr <$> constant
                  , IdentExpr <$> identifier
                  , StringExpr <$> cstr
                  , between lparen rparen (ParentExpr <$> expr) ] -- TODO parse parentheses

cstr :: GenParser st Text
cstr = do
  spaces
  _ <- char '"'
  s <- many1 $ satisfy (/= '"')
  _ <- char '"'
  return (pack s)

data Constant = IntConst Int
              | FloatConst Float
              | DoubleConst Double
              | CharConst Char
              -- | EnumConst
                deriving (Eq, Show)

constant :: GenParser st Constant
constant = choice [ CharConst <$> charLiteral
                  , try (FloatConst <$> float)
                  , try (DoubleConst <$> double)
                  , IntConst <$> int ]

-- Statements ----------------------------------------------------------
data Statement = JumpStmt Jump
               | ExprStmt ExpressionStmt
               | Compound CompoundStmt
                 deriving (Eq, Show)

statement :: GenParser st Statement
statement = choice [ ExprStmt <$> expressionStmt
                   , JumpStmt <$> jump ]


data ExpressionStmt = ExpressionStmt (Maybe Expression)
                      deriving (Eq, Show)

followedBy p end = p >>= \r -> end >> return r

semi :: GenParser st ()
semi = spaces >> char ';' >> spaces

expressionStmt :: GenParser st ExpressionStmt
expressionStmt = ExpressionStmt <$> (semi >> return Nothing) <|>
                 ExpressionStmt <$> ((expr `followedBy` semi) >>= \x -> return (Just x))

data CompoundStmt = CompoundStmt [Statement]
                    deriving (Eq, Show)

compoundstmt :: GenParser st CompoundStmt
compoundstmt = between lbracket rbracket (CompoundStmt <$> many statement)

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
