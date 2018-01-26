{-# LANGUAGE OverloadedStrings #-}

module Compiler
    ( process, pass
    ) where

import Data.Text              (Text, unpack, pack)
import Data.Either            (fromRight, isRight, fromLeft)
import Text.Parsec.Error      (ParseError)

import Parser                 (parse)
import AST                    (reduce)
import CodeGen.Assembly       (generate)
import Formatters.ATTAssembly (format)

process t = parse t >>= reduce >>= pass generate >>= pass format

pass :: (a -> b) -> a -> (Either l b)
pass f x = Right (f x)
