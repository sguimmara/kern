module Kern.AST.Parser.Literals
    ( literal
    ) where

import           Data.Int

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Text

import           Kern.AST
import           Kern.AST.Parser.Utils
import           Kern.AST.Analysis

literal :: GenParser ParserState Literal
literal = (NumLiteral <$> cchar) <|> (StringLiteral <$> cstring) <|> (NumLiteral <$> (try cfloat)) <|> (NumLiteral <$> cint)

cint :: GenParser st NumLit
cint = do
  sg <- optionMaybe (char '-')
  ds <- many1 digit
  su <- optionMaybe (oneOf "Ll")
  spaces
  case su of
    Just _  -> case sg of
      Just _  -> return (CInt64 (-(read ds :: Int64)))
      Nothing -> return (CInt64 (read ds :: Int64))
    Nothing -> case sg of
      Just _  -> return (CInt32 (-(read ds :: Int32)))
      Nothing -> return (CInt32 (read ds :: Int32))

cchar :: GenParser ParserState NumLit
cchar = trim $ between quote quote (CChar <$> anyChar)

cstring :: GenParser ParserState StrLit
cstring =
  trim $ between dquote dquote (StrLit <$> many (noneOf ['"']))

decimal :: GenParser st String
decimal = do
  sg <- option "" (string "-")
  ds <- many1 digit
  _  <- char '.'
  ts <- many1 digit
  return (sg ++ ds ++ "." ++ ts)

cfloat :: GenParser st NumLit
cfloat = (try cfloatSci) <|> cfloatLit

cfloatSci :: GenParser st NumLit
cfloatSci = do
  dec <- decimal
  e <- oneOf "Ee"
  s <- option "" (string "-")
  ds <- many1 digit
  su <- optionMaybe (oneOf "Ff")
  let val = dec ++ (e:s) ++ ds
  case su of
    Just _  -> return (CFloat32 (read val :: Float))
    Nothing -> return (CFloat64 (read val :: Double))

cfloatLit :: GenParser st NumLit
cfloatLit = do
  dec <- decimal
  su <- optionMaybe (oneOf "Ff")
  spaces
  case su of
    Just _  -> return (CFloat32 (read dec :: Float))
    Nothing -> return (CFloat64 (read dec :: Double))
