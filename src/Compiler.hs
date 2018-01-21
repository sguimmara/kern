{-# LANGUAGE OverloadedStrings #-}

module Compiler
    ( compile
    ) where

import Data.Text         (Text, unpack, pack)
import Text.Parsec       (parse, ParseError)
import Text.Parsec.Error (errorMessages, messageString)

import AST              (translunit)
import CodeGen

compile :: Text -> Either Text Text
compile txt = let p = parse translunit "" txt in
                case p of
                    Left err -> Left (pack $ unlines $ map messageString $ errorMessages err)
                    (Right ast) -> (Right . pack . toAssembly . emit) ast