module Kern.AST.Parser.Utils
  ( unsupported
  , quote, dquote
  , semi, colon, comma
  , lbrace, rbrace
  , lparen, rparen
  , lbracket, rbracket
  , star
  , equal
  , keyword
  , trim
  , reserved
  , dataType
  , punc
  , identifier, newIdentifier, existingIdentifier
  ) where
import qualified Data.Text as T

import           Text.Parsec
import           Text.Parsec.Text
import           Text.Parsec.Char

import           Kern.AST
import           Kern.AST.Analysis

reserved =
  [ "auto", "break", "case", "char"
  , "const", "continue", "default", "do"
  , "double", "else", "enum", "extern"
  , "float", "for", "goto", "if", "int"
  , "long", "register", "return", "short"
  , "signed", "sizeof", "static", "struct"
  , "switch", "typedef", "union", "unsigned"
  , "void", "volatile", "while"
  ]

unsupported :: String -> String
unsupported s = "unsupported: " ++ s

punc :: Char -> GenParser ParserState ()
punc c = char c >> spaces

quote    = char '\'' :: GenParser st Char
dquote   = char '"' :: GenParser st Char
colon    = punc ':'
semi     = punc ';'
lbrace   = punc '{'
rbrace   = punc '}'
lparen   = punc '('
rparen   = punc ')'
lbracket = punc '['
rbracket = punc ']'
star     = punc '*'
comma    = punc ','
equal    = punc '='

signs = "&|;:,?!/\\"

trim :: GenParser st a -> GenParser st a
trim p = p >>= \x -> spaces >> return x

keyword :: String -> GenParser ParserState String
keyword s =
  string s >> notFollowedBy (oneOf signs <|> alphaNum) >> spaces >> return s

newIdentifier :: GenParser ParserState Identifier
newIdentifier = do
  i@(Id t) <- identifier
  s <- getState
  if (isDeclared i $ symTable s)
    then fail ("identifier already declared: " ++ (T.unpack t))
    else do
      modifyState (declareIdentifier i)
      return i

existingIdentifier :: GenParser ParserState Identifier
existingIdentifier = do
  i@(Id t) <- identifier
  s <- getState
  if (isDeclared i $ symTable s)
    then return i
    else fail ("unknown name: " ++ (T.unpack t))

identifier :: GenParser ParserState Identifier
identifier = do
  x <- letter <|> char '_'
  xs <- many (alphaNum <|> char '_')
  spaces
  let name = (x:xs)
  if name `elem` reserved
    then fail (name ++ " is a reserved word")
    else return $ Id (T.pack name)

makeInt :: [TypeSpec] -> [TypeSpec] -> Either String DataType
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

dataType :: [TypeSpec] -> Either String DataType
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

isDataType :: TypeSpec -> Bool
isDataType t
  | t == SpecInt = True
  | t == SpecFloat = True
  | t == SpecChar = True
  | t == SpecDouble = True
  | t == SpecVoid = True
  | otherwise = False