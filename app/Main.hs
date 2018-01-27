{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           System.Exit
import qualified Data.ByteString as BS
import           Data.Yaml.Pretty
import           Data.Yaml
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text.IO as TIO    (readFile, putStr, putStrLn)
import           System.FilePath        (takeFileName)

import Compiler


main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> TIO.putStrLn "usage: kern FILE" >> exitWith (ExitFailure 1)
        ("--parse":path:[]) -> partial path toParseTree
        ("--ast":path:[]) -> partial path toAST

partial :: (Show a, ToJSON b) => FilePath -> (Text -> Either a b) -> IO ()
partial path f = do
  c <- TIO.readFile path
  let r = f c
  case r of
    (Left err) -> print err
    (Right ok) -> BS.putStr $ encodePretty defConfig $ toJSON ok
