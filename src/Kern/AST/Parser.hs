{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Kern.AST.Parser
    ( module Kern.AST.Parser.Utils
    , module Kern.AST.Parser.Literals
    , module Kern.AST.Parser.Declarations
    , module Kern.AST.Parser.Expressions
    , module Kern.AST.Parser.Statements
    , testParser
    ) where

import           Kern.AST
import           Kern.AST.Analysis
import           Kern.AST.Parser.Utils
import           Kern.AST.Parser.Literals
import           Kern.AST.Parser.Declarations
import           Kern.AST.Parser.Expressions
import           Kern.AST.Parser.Statements

import           Text.Parsec

testParser p s = runParser p mkState "<test>" s
