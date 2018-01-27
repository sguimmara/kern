{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parse
    , digits, int, float, decimal
    , Identifier (..), identifier
    , TranslationUnit (..), translationunit, emptyTranslationUnit
    , ExternalDeclaration (..), externaldecl
    , FunctionDefinition (..), functionDefinition
    , Declaration (..), declaration
    , Declarator (..), declarator
    , Pointer (..), pointer
    , DirectDeclarator (..), directDeclarator
    , InitDeclarator (..), initDeclarator
    , ParamList (..), paramlist
    , ParameterDeclaration (..), parameterDeclaration
    , DeclarationSpecifier (..), declarationSpecifier
    , StorageClassSpecifier (..), storageClassSpecifier
    , typeSpecifier
    , typeQualifier
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
    , CompoundStatement(..), compoundStatement
    , Jump (..), jump
    ) where

import           Core

import           Data.Either
import           Data.Maybe
import           Data.Text           (Text, unpack, pack)
import           Data.Yaml
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

parse :: Text -> Either ParseError TranslationUnit
parse = P.parse translationunit ""

------------------------------------------------------------------------
-- Grammar -------------------------------------------------------------
------------------------------------------------------------------------

newtype TranslationUnit = TranslationUnit  [ExternalDeclaration]
                          deriving (Eq, Show)

data ExternalDeclaration = ExtDeclFunc FunctionDefinition
                         | ExtDeclDecl Declaration
                           deriving (Eq, Show)

data FunctionDefinition =
  FunctionDefinition [DeclarationSpecifier]
                     Declarator
                     [Declaration]
                     CompoundStatement
                     deriving (Eq, Show)

data Declarator = Declarator (Maybe Pointer) DirectDeclarator
                  deriving (Eq, Show)

data Pointer = Pointer [TypeQualifier] (Maybe Pointer)
               deriving (Eq, Show)

data DirectDeclarator = DirectDeclId Identifier
                      | DirectDeclP Identifier ParamList --FIXME: it should be  DirectDeclP DirectDeclarator ParamList
                        deriving (Eq, Show)

data ParamList = ParamList [ParameterDeclaration]
                 deriving (Eq, Show)

data ParameterDeclaration =
  ParameterDeclaration [DeclarationSpecifier] Declarator
  deriving (Eq, Show)

data DeclarationSpecifier = DeclSpecStor StorageClassSpecifier
                          | DeclSpecSpec TypeSpecifier
                          | DeclSpecQual TypeQualifier
                            deriving (Eq, Show)

data StorageClassSpecifier = Auto
                           | Register
                           | Static
                           | Extern
                           | TypeDef
                           deriving (Eq, Show)

newtype Identifier = Identifier Text
                     deriving (Eq, Show)

data Expression = ExprAssignExpr AssignExpr
                  deriving (Eq, Show)

data AssignExpr = AssignCond CondExpr
                | AssignExprUn UnaryExpr AssignOp AssignExpr
                  deriving (Eq, Show)

data AssignOp = Assign
                deriving (Eq, Show)

data CondExpr = CondExprLogOrExpr LogicalOrExpr
                deriving (Eq, Show)

data LogicalOrExpr = LOEAnd LogicalAndExpr
                           deriving (Eq, Show)

data LogicalAndExpr = LAEInclusiveOr InclusiveOrExpr
                      deriving (Eq, Show)

data InclusiveOrExpr = IOEExclusiveOr ExclusiveOrExpr
                       deriving (Eq, Show)

data ExclusiveOrExpr = EOEAndExpr AndExpr
                       deriving (Eq, Show)

data AndExpr = AndExprEqual EqualExpr
               deriving (Eq, Show)

data EqualExpr = EqualExprRelat RelatExpr
                 deriving (Eq, Show)

data RelatExpr = RelatShift ShiftExpr
                 deriving (Eq, Show)

data ShiftExpr = ShiftExprAdd AdditiveExpr
                 deriving (Eq, Show)

data AdditiveExpr = AdditiveExprMul MultiplicativeExpr
                    deriving (Eq, Show)

data MultiplicativeExpr = MultiExprCast CastExpr
                          deriving (Eq, Show)

data CastExpr = CastExprUn UnaryExpr
                deriving (Eq, Show)

data UnaryExpr = UnaryExprPF PostfixExpr
                 deriving (Eq, Show)

data PostfixExpr = PFPrimeExpr PrimExpr
                   deriving (Eq, Show)

data PrimExpr = ConstExpr Constant
              | IdentExpr Identifier
              | StringExpr Text
              | ParentExpr Expression
                deriving (Eq, Show)

data Constant = IntConst Int
              | FloatConst Float
              | DoubleConst Double
              | CharConst Char
              -- | EnumConst
                deriving (Eq, Show)

-- Statements ----------------------------------------------------------
data Statement = JumpStmt Jump
               | ExprStmt ExpressionStmt
               | Compound CompoundStatement
                 deriving (Eq, Show)

data ExpressionStmt = ExpressionStmt (Maybe Expression)
                      deriving (Eq, Show)

data Declaration = Declaration [DeclarationSpecifier] [InitDeclarator]
                   deriving (Eq, Show)

data InitDeclarator = InitDeclarator Declarator (Maybe Initializer)
                      deriving (Eq, Show)

data Initializer = InitExpr AssignExpr
                 | InitList [Initializer]
                   deriving (Eq, Show)

data CompoundStatement = CompoundStatement [Declaration] [Statement]
                    deriving (Eq, Show)

data Jump = Goto Identifier
          | Continue
          | Break
          | Return (Maybe PrimExpr)
            deriving (Eq, Show)

------------------------------------------------------------------------
-- YAML representations ------------------------------------------------
------------------------------------------------------------------------

instance ToJSON TranslationUnit where
  toJSON (TranslationUnit ds) =
    object [ ("<translation-unit>", array $ map toJSON ds)]

instance ToJSON ExternalDeclaration where
  toJSON (ExtDeclFunc func) =
    object [("<function-definition>", toJSON func)]
  toJSON (ExtDeclDecl decl) =
    object [("<declaration>", toJSON decl)]

instance ToJSON FunctionDefinition where
  toJSON (FunctionDefinition spec dctor decls stmts) =
    object [ ("<declaration-specifier>", array $ map toJSON spec)
           , ("<declarator>", toJSON dctor)
           , ("<declaration>", array $ map toJSON decls)
           , ("<compound-statement>", toJSON stmts)
           ]

instance ToJSON Pointer where
  toJSON (Pointer qs p) =
    object ( ("<type-qualifier>", array $ map toJSON qs) : ptr )
      where ptr = if isNothing p then []
                  else [ ("<pointer>", toJSON $ fromJust p) ]

instance ToJSON DirectDeclarator where
  toJSON (DirectDeclId ident)  = object [ ("<identifier>", toJSON ident) ]
  toJSON (DirectDeclP decl ps) = object [ ("<direct-declarator>", toJSON decl)
                                        , ("<parameter-list>", toJSON ps)]

instance ToJSON DeclarationSpecifier where
  toJSON (DeclSpecStor stor) = object [ ("<storage-class-specifier>", toJSON stor)]
  toJSON (DeclSpecSpec spec) = object [ ("<type-specifier>", toJSON spec)]
  toJSON (DeclSpecQual qual) = object [ ("<type-qualifier>", toJSON qual)]

instance ToJSON StorageClassSpecifier where
  toJSON Auto     = String "auto"
  toJSON Register = String "register"
  toJSON Static   = String "static"
  toJSON Extern   = String "extern"
  toJSON TypeDef  = String "typedef"

instance ToJSON Identifier where
  toJSON (Identifier t) = String t

instance ToJSON ParamList where
  toJSON (ParamList ps) = array $ map toJSON ps

instance ToJSON ParameterDeclaration where
  toJSON (ParameterDeclaration specs decl) =
    object [ ("<declaration-specifier>", array $ map toJSON specs)
           , ("<declarator>", toJSON decl)]

instance ToJSON Declaration where
  toJSON (Declaration specs decls) =
    object [ ("<declaration-specifier>", array $ map toJSON specs)
           , ("<init-declarator>", array $ map toJSON decls)]

instance ToJSON InitDeclarator where
  toJSON (InitDeclarator decl ini) =
    object [ ("<declarator>", toJSON decl)
           , ("<initializer>", maybe Null toJSON ini)]

instance ToJSON Initializer where
  toJSON (InitExpr expr) =
    object [("<assignment-expression>", toJSON expr)]
  toJSON (InitList inis) = array $ map toJSON inis

instance ToJSON Declarator where
  toJSON (Declarator p ddecl) =
    object [ ("<pointer>", maybe Null toJSON p)
           , ("<direct-declarator>", toJSON ddecl) ]

instance ToJSON CompoundStatement where
  toJSON (CompoundStatement decls stmts) =
    object [ ("<declaration>", array $ map toJSON decls)
           , ("<statement>", array $ map toJSON stmts) ]

instance ToJSON Statement where
  toJSON (JumpStmt j) = object [ ("<jump-statement>", toJSON j) ]
  toJSON (ExprStmt e) = object [ ("<expression-statement>", toJSON e) ]
  toJSON (Compound s) = object [ ("<compound-statement>", toJSON s) ]

instance ToJSON ExpressionStmt where
  toJSON (ExpressionStmt e) = maybe Null toJSON e

instance ToJSON Expression where
  toJSON (ExprAssignExpr e) = Null --FIXME

instance ToJSON Jump where
  toJSON (Goto ident) = object [ ("<goto>", toJSON ident) ]
  toJSON Continue     = String "continue"
  toJSON Break        = String "break"
  toJSON (Return e)   = object [ ("<return>", toJSON e) ]

instance ToJSON PrimExpr where
  toJSON (ConstExpr e) = object [ ("<constant-expression>", toJSON e) ]
  toJSON (IdentExpr i) = object [ ("<identifier>", toJSON i) ]
  toJSON (StringExpr t) = String t
  toJSON (ParentExpr e) = Null --FIXME

instance ToJSON Constant where
  toJSON (IntConst i )   = object [ ("<integer-constant>", toJSON i) ]
  toJSON (FloatConst f)  = object [ ("<floating-constant>", toJSON f) ]
  toJSON (DoubleConst d) = object [ ("<double-constant>", toJSON d) ]
  toJSON (CharConst c)   = object [ ("<character-constant>", toJSON c) ]

instance ToJSON AssignExpr where
  toJSON (AssignCond cond)        = Null -- FIXME
  toJSON (AssignExprUn u op expr) = Null --FIXME

------------------------------------------------------------------------
-- Parsers -------------------------------------------------------------
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

comma = spaces >> char ',' >> spaces

spaced :: GenParser st a -> GenParser st a
spaced = between spaces spaces

emptyTranslationUnit = TranslationUnit []

translationunit :: GenParser st TranslationUnit
translationunit = TranslationUnit <$> many externaldecl

externaldecl :: GenParser st ExternalDeclaration
externaldecl = spaces >> choice [ try (ExtDeclFunc <$> functionDefinition)
                                , ExtDeclDecl <$> declaration ]

functionDefinition :: GenParser st FunctionDefinition
functionDefinition =
  spaces >> FunctionDefinition <$> (many declarationSpecifier)
                               <*> declarator
                               <*> (many declaration)
                               <*> compoundStatement

declaration :: GenParser st Declaration
declaration = (Declaration <$> (many1 declarationSpecifier)
                           <*> (many initDeclarator)) `followedBy` semi

initDeclarator :: GenParser st InitDeclarator
initDeclarator = InitDeclarator <$> declarator <*> (optionMaybe eq)
                 where eq = try ( (spaced (char '=')) >> initializer)

initializer :: GenParser st Initializer
initializer = spaces >> choice [ InitExpr <$> assignexpr
                               , InitList <$> initializerList ]

initializerList :: GenParser st [Initializer]
initializerList = initializer `sepBy` (spaces >> char ',')

declarator :: GenParser st Declarator
declarator = spaced (Declarator <$> (optionMaybe pointer)
                                <*> directDeclarator)

pointer :: GenParser st Pointer
pointer = spaced (char '*') >>
          Pointer <$> (many typeQualifier) <*> (optionMaybe pointer)

directDeclarator :: GenParser st DirectDeclarator
directDeclarator =
  spaced $ choice [ try (DirectDeclP <$> identifier <*> paramlist)
                  , DirectDeclId <$> identifier ]

paramlist :: GenParser st ParamList
paramlist = between lparen rparen
            (ParamList <$> parameterDeclaration `sepBy` comma)

parameterDeclaration :: GenParser st ParameterDeclaration
parameterDeclaration =
  choice [ ParameterDeclaration <$> (many declarationSpecifier)
                                <*> declarator ]

declarationSpecifier :: GenParser st DeclarationSpecifier
declarationSpecifier =
  spaced $ choice [ DeclSpecStor <$> storageClassSpecifier
                  , DeclSpecSpec <$> typeSpecifier
                  , DeclSpecQual <$> typeQualifier ]

typeSpecifier :: GenParser st TypeSpecifier
typeSpecifier = choice [ try $ string "void" >> return VoidT
                       , try $ string "short" >> return ShortT
                       , string "long" >> return LongT
                       , string "float" >> return FloatT
                       , string "double" >> return DoubleT
                       , string "signed" >> return SignedT
                       , string "unsigned" >> return UnsignedT
                       , string "int" >> return IntT ]

typeQualifier :: GenParser st TypeQualifier
typeQualifier = choice [ string "const" >> return ConstQ
                       , try $ string "volatile" >> return VolatileQ ]
                >>= \x -> spaces >> return x

storageClassSpecifier :: GenParser st StorageClassSpecifier
storageClassSpecifier =
  try $ choice [ string "auto" >> return Auto
               , string "register" >> return Register
               , try $ string "static" >> return Static
               , string "extern" >> return Extern
               , string "typedef" >> return TypeDef ]

identifier :: GenParser st Identifier
identifier = do
    spaces
    let initChars = '_' : letters
    x <- oneOf initChars
    xs <- many $ oneOf (digits ++ initChars)
    return $ Identifier (pack (x : xs))

expr :: GenParser st Expression
expr = spaces >> choice [ ExprAssignExpr <$> assignexpr ]

assignexpr :: GenParser st AssignExpr
assignexpr = spaces >>
             ((try (AssignExprUn <$> unaryexpr <*> assignop <*> assignexpr))
               <|> (AssignCond <$> condexpr))

assignop :: GenParser st AssignOp
assignop = spaces >> choice [ char '=' >> return Assign ]

condexpr :: GenParser st CondExpr
condexpr = spaces >> choice [ CondExprLogOrExpr <$> logicalOrExpr ]

logicalOrExpr :: GenParser st LogicalOrExpr
logicalOrExpr = spaces >> choice [ LOEAnd <$> logicalandexpr ]

logicalandexpr :: GenParser st LogicalAndExpr
logicalandexpr = spaces >> choice [ LAEInclusiveOr <$> inclusiveOrExpr ]

inclusiveOrExpr :: GenParser st InclusiveOrExpr
inclusiveOrExpr = spaces >> choice [ IOEExclusiveOr <$> exclusiveOrExpr ]

exclusiveOrExpr :: GenParser st ExclusiveOrExpr
exclusiveOrExpr = spaces >> choice [ EOEAndExpr <$> andexpr ]

andexpr :: GenParser st AndExpr
andexpr = spaces >> choice [ AndExprEqual <$> equalExpr ]

equalExpr :: GenParser st EqualExpr
equalExpr = spaces >> choice [ EqualExprRelat <$> relatExpr ]

relatExpr :: GenParser st RelatExpr
relatExpr = spaces >> choice [ RelatShift <$> shiftExpr ]

shiftExpr :: GenParser st ShiftExpr
shiftExpr = spaces >> choice [ ShiftExprAdd <$> additiveExpr ]

additiveExpr :: GenParser st AdditiveExpr
additiveExpr = spaces >> choice [ AdditiveExprMul <$> multiplicativeExpr ]

multiplicativeExpr :: GenParser st MultiplicativeExpr
multiplicativeExpr = spaces >> choice [ MultiExprCast <$> castExpr ]

castExpr :: GenParser st CastExpr
castExpr = spaces >> choice [ CastExprUn <$> unaryexpr]

unaryexpr :: GenParser st UnaryExpr
unaryexpr = choice [ UnaryExprPF <$> postfixexpr ]

postfixexpr :: GenParser st PostfixExpr
postfixexpr = choice [ PFPrimeExpr <$> primexpr ]

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

constant :: GenParser st Constant
constant = choice [ CharConst <$> charLiteral
                  , try (FloatConst <$> float)
                  , try (DoubleConst <$> double)
                  , IntConst <$> int ]

statement :: GenParser st Statement
statement = choice [ JumpStmt <$> jump
                   , ExprStmt <$> expressionStmt ]

followedBy p end = p >>= \r -> end >> return r

semi :: GenParser st ()
semi = spaces >> char ';' >> spaces

expressionStmt :: GenParser st ExpressionStmt
expressionStmt = ExpressionStmt <$> (semi >> return Nothing) <|>
                 ExpressionStmt <$> ((expr `followedBy` semi) >>= \x -> return (Just x))

compoundStatement :: GenParser st CompoundStatement
compoundStatement =
  between lbracket rbracket
    (CompoundStatement <$> many declaration <*> many statement)

jump :: GenParser st Jump
jump = do
  jmp <- choice [ string "goto" >> spaces >> Goto <$> identifier
                , string "continue" >> return Continue
                , string "break" >> return Break
                , string "return" >> spaces >> Return <$> (optionMaybe primexpr)
                ]
  spaces >> char ';' >> spaces
  return jmp