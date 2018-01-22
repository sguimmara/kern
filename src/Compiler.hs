{-# LANGUAGE OverloadedStrings #-}

module Compiler
    ( compile
    ) where

import Data.Text         (Text, unpack, pack)
import Text.Parsec       (parse, ParseError)
import Text.Parsec.Error (errorMessages, messageString)

import AST              (translunit)
import CodeGen
import Formatter        (prettyPrint)

compile :: FilePath -> Text -> Either Text Text
compile file txt = let p = parse (translunit file) "" txt in
                    case p of
                        Left err -> Left (pack $ unlines $ map messageString $ errorMessages err)
                        (Right ast) -> (Right . prettyPrint . emit) ast
