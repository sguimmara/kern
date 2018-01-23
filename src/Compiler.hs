{-# LANGUAGE OverloadedStrings #-}

module Compiler
    ( compile
    ) where

import Data.Text         (Text, unpack, pack)

import qualified AST as AST
import qualified Parser as P
import qualified CodeGen.Assembly as ASM
import qualified Formatters.ATTAssembly as FMT

compile :: Text -> Text
compile = FMT.format . ASM.gen . AST.reduce . P.parseForce

