{-# LANGUAGE OverloadedStrings #-}

module Kern.AST.Parser where

import           Kern.Core
import           Kern.AST

import           Data.Int
import           Data.Either
import           Data.List
import           Data.Text        (pack)
import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Text

whitespace :: GenParser st ()
whitespace = ((space >> spaces) <|> eof <|> lookAhead (oneOf ['*', ';', ','] >> return ()))

manyOrd :: Ord a => ParsecT s u m a -> ParsecT s u m [a]
manyOrd p = do
  xs <- many p
  return (sort $ nub xs)

keyword :: String -> GenParser st ()
keyword s = string s >> whitespace

symbol :: String -> GenParser st ()
symbol s = spaces >> string s >> spaces

typeSpecifier :: GenParser st TypeSpecifier
typeSpecifier = (try $ (keyword "short" >> return SpecShort)) <|>
                (try $ (keyword "long" >> return SpecLong)) <|>
                (try $ (keyword "char" >> return SpecChar)) <|>
                (try $ (keyword "int" >> return SpecInt)) <|>
                (try $ (keyword "float" >> return SpecFloat)) <|>
                (try $ (keyword "double" >> return SpecDouble)) <|>
                (try $ (keyword "void" >> return SpecVoid)) <|>
                (try $ (keyword "signed" >> return SpecSigned)) <|>
                (try $ (keyword "unsigned" >> return SpecUnsigned))

storageClass :: GenParser st StorageClass
storageClass = (try $ keyword "auto" >> return Auto) <|>
               (try $ keyword "register" >> return Register) <|>
               (try $ keyword "static" >> return Static) <|>
               (try $ keyword "extern" >> return Extern) <|>
               (try $ keyword "typedef" >> return Typedef)

typeQualifier :: GenParser st TypeQualifier
typeQualifier = (try $ keyword "const" >> return ConstQualifier) <|>
                (try $ keyword "volatile" >> return VolatileQualifier)

declarationSpecifier :: GenParser st DeclarationSpecifier
declarationSpecifier = (StorageClass <$> storageClass) <|>
                       (TypeSpecifier <$> typeSpecifier) <|>
                       (TypeQualifier <$> typeQualifier)

separateDeclSpecs ::
     [DeclarationSpecifier]
  -> ([StorageClass], [TypeSpecifier], [TypeQualifier])
separateDeclSpecs [] = ([],[],[])
separateDeclSpecs xs = f xs [] [] []
  where f (StorageClass s:xs) sto spec qual  = f xs (s:sto) spec qual
        f (TypeSpecifier t:xs) sto spec qual = f xs sto (t:spec) qual
        f (TypeQualifier q:xs) sto spec qual = f xs sto spec (q:qual)
        f [] sto spec qual                   = (sto, spec, qual)

isDataType :: TypeSpecifier -> Bool
isDataType t
  | t == SpecInt = True
  | t == SpecFloat = True
  | t == SpecChar = True
  | t == SpecDouble = True
  | t == SpecVoid = True
  | otherwise = False

makeInt :: [TypeSpecifier] -> [TypeSpecifier] -> Either String DataType
makeInt [] [] = Right TyInt32
makeInt [] [SpecUnsigned] = Right TyUint32
makeInt [] [SpecSigned] = Right TyInt32
makeInt [SpecLong] [] = Right TyInt64
makeInt [SpecShort] [] = Right TyInt16
makeInt [SpecShort] [SpecSigned] = Right TyInt16
makeInt _ [SpecSigned, SpecSigned] =
  Left $ "duplicate 'signed'"
makeInt _ [SpecUnsigned, SpecUnsigned] =
  Left $ "duplicate 'unsigned'"
makeInt [SpecShort, SpecShort] _ =
  Left $ "duplicate 'short'"
makeInt [SpecLong, SpecLong, SpecLong] _ =
  Left $ "'long long long' is too long"
makeInt [SpecShort] [SpecUnsigned] = Right TyUint16
makeInt [SpecLong] [SpecSigned] = Right TyInt64
makeInt [SpecLong, SpecLong] [SpecSigned] = Right TyInt64
makeInt [SpecLong, SpecLong] [] = Right TyInt64
makeInt [SpecLong] [SpecUnsigned] = Right TyUint64
makeInt [SpecLong, SpecLong] [SpecUnsigned] = Right TyUint64

dataType :: [TypeSpecifier] -> Either String DataType
dataType [SpecVoid] = Right TyVoid
dataType [SpecFloat] = Right TyFloat32
dataType [SpecDouble] = Right TyFloat64
dataType [SpecInt] = Right TyInt32
dataType [SpecChar] = Right TyByte
dataType xs =
  let ln = filter (== SpecLong) xs
      sh = filter (== SpecShort) xs
      si = filter (== SpecSigned) xs
      un = filter (== SpecUnsigned) xs
      dt = filter isDataType xs
      errBoth a b =
        Left $ "both '" ++ a ++ "' and '" ++ b ++
               "' in declaration specifiers" in
      if (not $ null ln) && (not $ null sh) then
        errBoth "long" "short"
      else if (not $ null si) && (not $ null un) then
        errBoth "signed" "unsigned"
      else if length dt > 1 then
        Left "two or more data types in declaration specifiers"
      else case dt of
        [] -> makeInt (ln ++ sh) (si ++ un)
        [SpecInt] -> makeInt (ln ++ sh) (si ++ un)
        [SpecFloat] -> Left "two or more data types in declaration specifiers"

keywords =
  [ "auto", "break", "case", "char"
  , "const", "continue", "default", "do"
  , "double", "else", "enum", "extern"
  , "float", "for", "goto", "if", "int"
  , "long", "register", "return", "short"
  , "signed", "sizeof", "static", "struct"
  , "switch", "typedef", "union", "unsigned"
  , "void", "volatile", "while"
  ]

identifier :: GenParser st Identifier
identifier = try $ do
  x <- letter <|> char '_' <?> "identifier"
  xs <- many (alphaNum <|> char '_')
  spaces
  let res = (x:xs)
  if res `elem` keywords then fail ""
  else return $ Ident (pack res)

getFuncLinkage :: [StorageClass] -> Either String Linkage
getFuncLinkage [] = Right External
getFuncLinkage [Extern] = Right External
getFuncLinkage [Static] = Right Internal
getFuncLinkage [Register] =
  Left $ "'register' storage class not allowed for function definitions"
getFuncLinkage [Typedef] =
  Left $ "function definition declared 'typedef'"
getFuncLinkage [Auto] =
  Left $ "function definition declared 'auto'"

getFuncQualifiers :: [TypeQualifier] -> (Constness, Volatility)
getFuncQualifiers [] = (Mutable, NonVolatile)
getFuncQualifiers xs = (m, v) where
  m    = if ConstQualifier `elem` xs then Constant else Mutable
  v    = if VolatileQualifier `elem` xs then Volatile else NonVolatile

returnType :: GenParser st ExternalType
returnType = do
  decls <- many declarationSpecifier
  ptr <- optionMaybe pointer
  let (sto, spec, quals) = separateDeclSpecs decls
  let ind = case ptr of
              Nothing -> Direct
              Just p -> Indirect p

  let err1 s = fail ("function definition declared '" ++ s ++ "'")
  if Register `elem` sto then err1 "register"
  else if Typedef `elem` sto then err1 "typedef"
  else if Auto `elem` sto then err1 "auto"
  else do
    let dt = dataType spec
        sc = getFuncLinkage sto
        (cnst, vol) = getFuncQualifiers quals
    case dt of
      (Left err) -> fail err
      (Right dt') -> case sc of
        (Left err)  -> fail err
        (Right sc') -> return $ ExternalType dt' sc' cnst vol ind

paramType :: GenParser st ParameterType
paramType = do
  decls <- many declarationSpecifier
  ptr <- optionMaybe pointer
  let (sto, spec, quals) = separateDeclSpecs decls
  let ind = case ptr of
              Nothing -> Direct
              Just p -> Indirect p

  if (not . null) sto then fail "storage class specifier for parameter"
  else do
    let dt = dataType spec
        (cnst, vol) = getFuncQualifiers quals
    case dt of
      (Left err) -> fail err
      (Right dt') -> return $ ParameterType dt' cnst vol ind

internalType :: GenParser st InternalType
internalType = do
  decls <- many1 declarationSpecifier
  ptr <- optionMaybe pointer
  let (sto, spec, quals) = separateDeclSpecs decls
  let ind = case ptr of
              Nothing -> Direct
              Just p -> Indirect p

  let dt = dataType spec
      (cnst, vol) = getFuncQualifiers quals
  case dt of
    (Left err) -> fail err
    (Right dt') -> return $ InternalType dt' cnst vol ind

semi = char ';' >> spaces

lbracket = char '{' >> spaces :: GenParser st ()
rbracket = char '}' >> spaces :: GenParser st ()

lparen = char '(' >> spaces :: GenParser st ()
rparen = char ')' >> spaces :: GenParser st ()

commaSep :: GenParser st a -> GenParser st [a]
commaSep p =
  try ( do
      x <- p
      xs <- many $ spaces >> char ',' >> spaces >> p
      return (x:xs)
  ) <|> return []

param :: GenParser st Parameter
param = Parameter <$> paramType <*> identifier

endedBy p e = p >>= \r -> e >> return r

paramList :: GenParser st Params
paramList = between lparen rparen (commaSep param)

initializer = symbol "=" >> InitExpr <$> expr

extDeclaration :: GenParser st ExternalDeclaration
extDeclaration = choice [ FunctionDefinition <$> functionDefinition ]

translationUnit :: GenParser st TranslationUnit
translationUnit = TranslationUnit <$> (many extDeclaration)

localDeclaration :: GenParser st LocalVariable
localDeclaration = try ((LocalVariable <$> internalType <*> identifier <*>
                   (optionMaybe initializer)) `endedBy` semi)

statement :: GenParser st Statement
statement = choice [ Jump <$> jump
                   , ExprStmt <$> (optionMaybe expr) `endedBy` semi
                   ]

functionBody :: GenParser st Body
functionBody =
  between lbracket rbracket
          (Body <$> (many localDeclaration) <*> (many statement))

functionDefinition :: GenParser st FunctionDefinition
functionDefinition = Function <$> returnType
                              <*> identifier
                              <*> paramList
                              <*> functionBody

functionPrototype :: GenParser st FunctionPrototype
functionPrototype =
  (Prototype <$> returnType <*> identifier <*> paramList) `endedBy` semi
  <?> "function prototype"

pointer :: GenParser st Pointer
pointer = char '*' >> spaces >> many typeQualifier >>
          Pointer <$> (optionMaybe pointer)

jump :: GenParser st Jump
jump = choice [ keyword "goto" >> Goto <$> identifier
              , keyword "continue" >> return Continue
              , keyword "break" >> return Break
              , keyword "return" >> Return <$> (optionMaybe expr)
              ] `endedBy` semi

int32 :: GenParser st Int32
int32 = do
  ds <- many1 digit
  spaces
  case ds of
    ""  -> fail "integer"
    dds -> return (read ds :: Int32)

int64 :: GenParser st Int64
int64 = do
  ds <- many1 digit
  suf <- oneOf "Ll"
  spaces
  case ds of
    ""  -> fail "integer"
    dds -> return (read ds :: Int64)

float32 :: GenParser st Float
float32 = do
  ds <- many1 digit
  _ <- char '.'
  dds <- many1 digit
  suf <- char 'f'
  spaces
  return (read (ds ++ "." ++ dds) :: Float)

float64 :: GenParser st Double
float64 = do
  ds <- many1 digit
  _ <- char '.'
  dds <- many1 digit
  spaces
  return (read (ds ++ "." ++ dds) :: Double)

charLit :: GenParser st Char
charLit = between (char '\'') (char '\'') anyChar

literal :: GenParser st Literal
literal = (try $ Float32Lit <$> float32) <|>
          (try $ Float64Lit <$> float64) <|>
          (try $ Int64Lit <$> int64) <|>
          (Int32Lit <$> int32) <|>
          (CharLit <$> charLit) <?> "literal"

postfixExpr :: GenParser st Expr
postfixExpr =
  (try $ (PostfixInc <$> primExpr) `endedBy` (string "++")) <|>
  (try $ (PostfixDec <$> primExpr) `endedBy` (string "--")) <|>
  primExpr

primExpr :: GenParser st Expr
primExpr = (between lparen rparen expr) <|>
           (PrimI <$> identifier) <|>
           (PrimC <$> literal)

op :: GenParser st Op
op = choice [ symbol "=" >> return Equal
            , symbol "*=" >> return MulEq
            , symbol "/=" >> return DivEq
            , symbol "%=" >> return ModEq
            , symbol "+=" >> return AddEq
            , symbol "-=" >> return SubEq
            , symbol "<<=" >> return ShLEq
            , symbol ">>=" >> return ShREq
            , symbol "&=" >> return AndEq
            , symbol "^=" >> return XorEq
            , symbol "|=" >> return OrEq
            ]

expr :: GenParser st Expr
expr = assignExpr

condExpr :: GenParser st Expr
condExpr =
  (try (CondExpr <$> orExpr <*> (symbol "?" >> expr)
                            <*> (symbol ":" >> condExpr))) <|>
  orExpr

orExpr :: GenParser st Expr
orExpr =
  (try (Or <$> andExpr <*> (symbol "||" >> orExpr))) <|>
  andExpr

andExpr :: GenParser st Expr
andExpr =
  (try (And <$> bwOrExpr <*> (symbol "&&" >> andExpr))) <|>
  bwOrExpr

bwOrExpr :: GenParser st Expr
bwOrExpr =
  (try (BitwiseOr <$> xorExpr <*> (symbol "|" >> bwOrExpr))) <|>
  xorExpr

xorExpr :: GenParser st Expr
xorExpr =
  (try (Xor <$> bwAndExpr <*> (symbol "^" >> xorExpr))) <|>
  bwAndExpr

bwAndExpr :: GenParser st Expr
bwAndExpr =
  (try (BitwiseAnd <$> equalExpr <*> (symbol "&" >> bwAndExpr))) <|>
  equalExpr

equalExpr :: GenParser st Expr
equalExpr =
  (try (EqExpr <$> relat <*> (symbol "==" >> equalExpr))) <|>
  (try (NeqExpr <$> relat <*> (symbol "!=" >> equalExpr))) <|>
  relat

relat :: GenParser st Expr
relat =
  (try (Lt <$> shift <*> (symbol "<" >> relat))) <|>
  (try (Gt <$> shift <*> (symbol ">" >> relat))) <|>
  (try (LtEq <$> shift <*> (symbol "<=" >> relat))) <|>
  (try (GtEq <$> shift <*> (symbol ">=" >> relat))) <|>
  shift

shift :: GenParser st Expr
shift =
  (try (ShiftL <$> add <*> (symbol "<<" >> shift))) <|>
  (try (ShiftR <$> add <*> (symbol ">>" >> shift))) <|>
  add

add :: GenParser st Expr
add =
  (try (AddExpr <$> mul <*> (symbol "+" >> add))) <|>
  (try (SubExpr <$> mul <*> (symbol "-" >> add))) <|>
  mul

mul :: GenParser st Expr
mul =
  (try (MulExpr <$> castExpr <*> (symbol "*" >> mul))) <|>
  (try (DivExpr <$> castExpr <*> (symbol "/" >> mul))) <|>
  (try (ModExpr <$> castExpr <*> (symbol "%" >> mul))) <|>
  castExpr

castExpr = unaryExpr

unaryExpr :: GenParser st Expr
unaryExpr =
  postfixExpr <|>
  (try (PrefixInc <$> (symbol "++" >> unaryExpr))) <|>
  (try (PrefixDec <$> (symbol "--" >> unaryExpr))) <|>
  (symbol "-" >> Neg <$> castExpr) <|>
  (symbol "+" >> Plus <$> castExpr) <|>
  (symbol "&" >> AddrOf <$> castExpr) <|>
  (symbol "*" >> Deref <$> castExpr) <|>
  (symbol "!" >> Not <$> castExpr) <|>
  (symbol "~" >> Compl <$> castExpr) <?> "unary expression"

assignExpr :: GenParser st Expr
assignExpr = (try (Assign <$> unaryExpr <*> op <*> condExpr)) <|> condExpr
