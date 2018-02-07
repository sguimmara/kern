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

symbol :: String -> GenParser st String
symbol s = do
  res <- string s
  notFollowedBy (oneOf s)
  return res

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
statement = choice [ ExprStmt <$> (optionMaybe expr) `endedBy` semi
                   , Jump <$> jump
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

charLit :: GenParser st Char
charLit = between (char '\'') (char '\'' >> spaces) anyChar

decimal :: GenParser st String
decimal = do
  sg <- option "" (string "-")
  ds <- many1 digit
  _  <- char '.'
  ts <- many1 digit
  return (sg ++ ds ++ "." ++ ts)

floatSci :: GenParser st Literal
floatSci = do
  dec <- decimal
  e <- oneOf "Ee"
  s <- option "" (string "-")
  ds <- many1 digit
  su <- optionMaybe (oneOf "Ff")
  let val = dec ++ (e:s) ++ ds
  case su of
    Just _  -> return (Float32Lit (read val :: Float))
    Nothing -> return (Float64Lit (read val :: Double))

float :: GenParser st Literal
float = do
  dec <- decimal
  su <- optionMaybe (oneOf "Ff")
  spaces
  case su of
    Just _  -> return (Float32Lit (read dec :: Float))
    Nothing -> return (Float64Lit (read dec :: Double))

int :: GenParser st Literal
int = do
  sg <- optionMaybe (char '-')
  ds <- many1 digit
  su <- optionMaybe (oneOf "Ll")
  spaces
  case su of
    Just _  -> case sg of
      Just _  -> return (Int64Lit (-(read ds :: Int64)))
      Nothing -> return (Int64Lit (read ds :: Int64))
    Nothing -> case sg of
      Just _  -> return (Int32Lit (-(read ds :: Int32)))
      Nothing -> return (Int32Lit (read ds :: Int32))

literal :: GenParser st Literal
literal = (try floatSci) <|> (try float) <|> (try int) <|>
          (CharLit <$> charLit) <?> "literal"

postfixExpr :: GenParser st Expr
postfixExpr = do
  e <- primExpr
  s <- optionMaybe (try (string "++" <|> string "--"))
  case s of
    Nothing -> return e
    Just "++" -> return (PostfixInc e)
    Just "--" -> return (PostfixDec e)

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
condExpr = do
  e <- orExpr
  spaces
  s <- optionMaybe (try (symbol "?"))
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- expr
      _  <- symbol ":"
      e2 <- condExpr
      return (CondExpr e e1 e2)

orExpr :: GenParser st Expr
orExpr = do
  e <- andExpr
  s <- optionMaybe (try (symbol "||"))
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- orExpr
      return (Or e e1)

andExpr :: GenParser st Expr
andExpr = do
  e <- bwOrExpr
  s <- optionMaybe (try (symbol "&&"))
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- andExpr
      return (And e e1)

bwOrExpr :: GenParser st Expr
bwOrExpr = do
  e <- xorExpr
  s <- optionMaybe (try $ symbol "|")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- bwOrExpr
      return (BitwiseOr e e1)

xorExpr :: GenParser st Expr
xorExpr = do
  e <- bwAndExpr
  s <- optionMaybe (try $ symbol "^")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- xorExpr
      return (Xor e e1)

bwAndExpr :: GenParser st Expr
bwAndExpr = do
  e <- equalExpr
  s <- optionMaybe (try $ symbol "&")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- bwAndExpr
      return (BitwiseAnd e e1)

equalExpr :: GenParser st Expr
equalExpr = do
  e <- relat
  spaces
  s <- optionMaybe (try (string "==" <|> string "!="))
  spaces
  case s of
    Nothing -> return e
    Just "=="  -> EqExpr e <$> equalExpr
    Just "!="  -> NeqExpr e <$> equalExpr

relat :: GenParser st Expr
relat = do
  e <- shift
  spaces
  s <- optionMaybe (try (string "<=" <|> string ">=" <|> string "<" <|> string ">"))
  spaces
  case s of
    Nothing -> return e
    Just "<"  -> Lt e <$> relat
    Just ">"  -> Gt e <$> relat
    Just ">=" -> GtEq e <$> relat
    Just "<=" -> LtEq e <$> relat

shift :: GenParser st Expr
shift = do
  e <- add
  spaces
  s <- optionMaybe (try (string "<<" <|> string ">>"))
  spaces
  case s of
    Nothing -> return e
    Just "<<" -> ShiftL e <$> shift
    Just ">>" -> ShiftR e <$> shift

add :: GenParser st Expr
add = do
  e <- mul
  spaces
  s <- optionMaybe (try (string "+" <|> string "-"))
  spaces
  case s of
    Nothing -> return e
    Just "+" -> AddExpr e <$> add
    Just "-" -> SubExpr e <$> add

mul :: GenParser st Expr
mul = do
  e <- castExpr
  spaces
  s <- optionMaybe (try (string "*" <|> string "/" <|> string "%"))
  spaces
  case s of
    Nothing -> return e
    Just "*" -> MulExpr e <$> mul
    Just "/" -> DivExpr e <$> mul
    Just "%" -> ModExpr e <$> mul

-- FIXME
castExpr = unaryExpr

unaryExpr :: GenParser st Expr
unaryExpr =
  (try postfixExpr) <|>
  (try unaryPostfix) <|>
  (try unaryPrefix)

unaryPostfix :: GenParser st Expr
unaryPostfix = do
  s <- string "++" <|> string "--"
  e <- unaryExpr
  case s of
    "++" -> return (PrefixInc e)
    "--" -> return (PrefixDec e)

unaryPrefix :: GenParser st Expr
unaryPrefix = do
  s <- oneOf "-+&*!~"
  e <- castExpr
  case s of
    '-' ->
      case e of
        (PrimC (Float32Lit n)) -> return (PrimC (Float32Lit (-n)))
        (PrimC (Float64Lit n)) -> return (PrimC (Float64Lit (-n)))
        (PrimC (Int32Lit n))   -> return (PrimC (Int32Lit (-n)))
        (PrimC (Int64Lit n))   -> return (PrimC (Int64Lit (-n)))
        _                      -> return (Neg e)
    '+' -> return (Plus e)
    '&' -> return (AddrOf e)
    '*' -> return (Deref e)
    '!' -> return (Not e)
    '~' -> return (Compl e)

assignExpr :: GenParser st Expr
assignExpr = (try (Assign <$> unaryExpr <*> op <*> condExpr)) <|> condExpr
