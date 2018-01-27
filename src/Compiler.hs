{-# LANGUAGE OverloadedStrings #-}

module Compiler
    ( process
    , toParseTree
    , toAST
    , toAssembly
    , pass
    , printYaml
    ) where

import           Data.Text              (Text, unpack, pack)
import qualified Data.ByteString as BS  (putStr)
import           Data.Either            (fromRight, isRight, fromLeft)
import           Data.Yaml              (ToJSON, toJSON)
import           Data.Yaml.Pretty       (encodePretty, defConfig)
import           Text.Parsec.Error      (ParseError)

import           Parser                 (parse, TranslationUnit)
import           AST                    (AST, reduce)
import           Arch.X64               (generate, Instr)
import           Syntax.ATandT          (format)

process :: Text -> Either ParseError Text
process t = parse t >>= reduce >>= pass generate >>= pass format

toParseTree :: Text -> Either ParseError TranslationUnit
toParseTree = parse

toAST :: Text -> Either ParseError AST
toAST t = parse t >>= reduce

toAssembly :: Text -> Either ParseError [Instr]
toAssembly t = parse t >>= reduce >>= pass generate

printYaml :: ToJSON a => a -> IO ()
printYaml y = BS.putStr $ encodePretty defConfig $ toJSON y

pass :: (a -> b) -> a -> (Either l b)
pass f x = Right (f x)
