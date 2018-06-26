module Kern.AST.Parser.Statements where

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Text

import           Kern.AST
import           Kern.AST.Analysis
import           Kern.AST.Parser.Utils
import           Kern.AST.Parser.Expressions

statement :: GenParser ParserState Statement
statement = (try jump) <|> (try exprStmt) <|> labeledStmt

jump :: GenParser ParserState Statement
jump = do
  kw <- string "goto"
        <|> string "continue"
        <|> string "break"
        <|> string "return"
  spaces
  case kw of
    "goto" -> (Goto <$> identifier) >>= \x -> semi >> return x
    "continue" -> semi >> return Continue
    "break" -> semi >> return Break
    "return" -> do
      e <- optionMaybe expr
      semi
      return (Return e)
    _ -> undefined

exprStmt :: GenParser ParserState Statement
exprStmt = do
  e <- optionMaybe expr
  semi
  return (ExprStmt e)

labeledStmt :: GenParser ParserState Statement
labeledStmt = (try caseStmt) <|> (try defaultStmt) <|> labelStmt

labelStmt :: GenParser ParserState Statement
labelStmt = do
  i <- identifier
  char ':'
  spaces
  s <- statement
  return (LabelStmt i s)

defaultStmt :: GenParser ParserState Statement
defaultStmt = do
  string "default"
  spaces
  char ':'
  spaces
  DefaultStmt <$> statement

caseStmt :: GenParser ParserState Statement
caseStmt = do
  string "case"
  spaces
  e <- condExpr
  spaces
  char ':'
  spaces
  s <- statement
  return (Case e s)