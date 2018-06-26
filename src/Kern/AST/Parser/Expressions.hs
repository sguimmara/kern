module Kern.AST.Parser.Expressions where

import           Data.Maybe

import           Text.Parsec
import           Text.Parsec.Text

import           Kern.AST
import           Kern.AST.Analysis
import           Kern.AST.Parser.Literals
import           Kern.AST.Parser.Utils

expr :: GenParser ParserState Expr
expr = assignExpr

assignExpr :: GenParser ParserState Expr
assignExpr =
  (try (Assign <$> unaryExpr <*> op <*> assignExpr)) <|> condExpr

condExpr :: GenParser ParserState Expr
condExpr = do
  e <- orExpr
  spaces
  s <- optionMaybe (try (keyword "?"))
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- expr
      _  <- colon
      e2 <- condExpr
      return (CondExpr e e1 e2)

orExpr :: GenParser ParserState Expr
orExpr = do
  e <- andExpr
  s <- optionMaybe (try (keyword "||"))
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- orExpr
      return (Or e e1)

andExpr :: GenParser ParserState Expr
andExpr = do
  e <- bwOrExpr
  s <- optionMaybe (try (keyword "&&"))
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- andExpr
      return (And e e1)

bwOrExpr :: GenParser ParserState Expr
bwOrExpr = do
  e <- xorExpr
  s <- optionMaybe (try $ keyword "|")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- bwOrExpr
      return (BitwiseOr e e1)

xorExpr :: GenParser ParserState Expr
xorExpr = do
  e <- bwAndExpr
  s <- optionMaybe (try $ keyword "^")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- xorExpr
      return (Xor e e1)

bwAndExpr :: GenParser ParserState Expr
bwAndExpr = do
  e <- equalExpr
  s <- optionMaybe (try $ keyword "&")
  spaces
  case s of
    Nothing -> return e
    Just _  -> do
      e1 <- bwAndExpr
      return (BitwiseAnd e e1)

equalExpr :: GenParser ParserState Expr
equalExpr = do
  e <- relat
  spaces
  s <- optionMaybe (try (keyword "==" <|> keyword "!="))
  spaces
  case s of
    Nothing -> return e
    Just "=="  -> EqExpr e <$> equalExpr
    Just "!="  -> NeqExpr e <$> equalExpr

relat :: GenParser ParserState Expr
relat = do
  e <- shift
  spaces
  x <- optionMaybe $ try (char '<' <|> char '>')
  if isNothing x
    then return e
    else do
      xx <- optionMaybe $ try (char '=')
      spaces
      let sym = (x:xx:[])
      case sym of
        (Just '>':Nothing:[]) -> Gt e <$> relat
        (Just '<':Nothing:[]) -> Lt e <$> relat
        (Just '<':Just '=':[]) -> LtEq e <$> relat
        (Just '>':Just '=':[]) -> GtEq e <$> relat

shift :: GenParser ParserState Expr
shift = do
  e <- add
  spaces
  s <- optionMaybe (try (keyword "<<" <|> keyword ">>"))
  spaces
  case s of
    Nothing -> return e
    Just "<<" -> ShiftL e <$> shift
    Just ">>" -> ShiftR e <$> shift

add :: GenParser ParserState Expr
add = do
  e <- mul
  spaces
  s <- optionMaybe (try (char '+' <|> char '-'))
  spaces
  case s of
    Nothing -> return e
    Just '+' -> AddExpr e <$> add
    Just '-' -> SubExpr e <$> add

mul :: GenParser ParserState Expr
mul = do
  e <- castExpr
  spaces
  s <- optionMaybe $ try $ oneOf "*/%"
  spaces
  case s of
    Nothing -> return e
    Just '*' -> MulExpr e <$> mul
    Just '/' -> DivExpr e <$> mul
    Just '%' -> ModExpr e <$> mul

castExpr :: GenParser ParserState Expr
castExpr = unaryExpr --FIXME

postfixExpr :: GenParser ParserState Expr
postfixExpr = do
  e <- primExpr
  s <- optionMaybe (try (string "++" <|> string "--"))
  spaces
  case s of
    Nothing -> return e
    Just "++" -> return (PostfixInc e)
    Just "--" -> return (PostfixDec e)

unaryExpr :: GenParser ParserState Expr
unaryExpr =
  (try prefixExpr) <|>
  (try postfixExpr) <|>
  (try unaryPrefix)

prefixExpr :: GenParser ParserState Expr
prefixExpr = do
  s <- string "++" <|> string "--"
  e <- unaryExpr
  spaces
  case s of
    "++" -> return (PrefixInc e)
    "--" -> return (PrefixDec e)


unaryPrefix :: GenParser ParserState Expr
unaryPrefix = do
  s <- oneOf "-+&*!~"
  e <- castExpr
  spaces
  case s of
    '-' ->
      case e of
        (PrimC (CFloat32 n)) -> return (PrimC (CFloat32 (-n)))
        (PrimC (CFloat64 n)) -> return (PrimC (CFloat64 (-n)))
        (PrimC (CInt32 n))   -> return (PrimC (CInt32 (-n)))
        (PrimC (CInt64 n))   -> return (PrimC (CInt64 (-n)))
        _                    -> return (Neg e)
    '+' -> return (Plus e)
    '&' -> return (AddrOf e)
    '*' -> return (Deref e)
    '!' -> return (Not e)
    '~' -> return (Compl e)

primExpr :: GenParser ParserState Expr
primExpr = (between lparen rparen expr) <|>
           (PrimI <$> identifier) <|>
           (PrimC <$> literal)

op :: GenParser ParserState Op
op =
  choice [ string "=" >> spaces >> return Equal
         , string "*=" >> spaces >> return MulEq
         , string "/=" >> spaces >> return DivEq
         , string "%=" >> spaces >> return ModEq
         , string "+=" >> spaces >> return AddEq
         , string "-=" >> spaces >> return SubEq
         , string "<<=" >> spaces >> return ShLEq
         , string ">>=" >> spaces >> return ShREq
         , string "&=" >> spaces >> return AndEq
         , string "^=" >> spaces >> return XorEq
         , string "|=" >> spaces >> return OrEq
         ]