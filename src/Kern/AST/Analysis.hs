module Kern.AST.Analysis where

import           Kern.AST

import           Data.List

data SymTable
  = SymTable [Identifier]
  deriving (Eq, Show)

isDeclared :: Identifier -> SymTable -> Bool
isDeclared i (SymTable is) = any (\i' -> i == i') is

addIdentifier :: Identifier -> SymTable -> SymTable
addIdentifier i (SymTable is) = SymTable (i:is)

declareIdentifier :: Identifier -> ParserState -> ParserState
declareIdentifier i (ParserState st) =
  ParserState st'
  where st' = addIdentifier i st

data ParserState
  = ParserState { symTable :: SymTable }
  deriving (Eq, Show)

mkState :: ParserState
mkState = ParserState (SymTable [])

