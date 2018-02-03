{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           System.Exit
import qualified Data.ByteString as BS
import           Data.Yaml.Pretty
import           Text.Parsec     as P
import           Data.Yaml
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text.IO as TIO    (readFile, putStr, putStrLn)
import           System.FilePath        (takeFileName)

import           Kern.Grammar
import           Kern.Grammar.Parser

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("--parse":path:[]) -> parseOnly path
        _                   -> printUsage

printUsage :: IO ()
printUsage = do
    TIO.putStrLn "usage: kern [--parse] FILE"
    exitWith (ExitFailure 1)

parseOnly :: FilePath -> IO ()
parseOnly path = do
    c <- TIO.readFile path
    let res = P.parse translationUnit path c
    case res of
        (Left err) -> print err
        (Right ok) -> BS.putStr $ encodePretty defConfig $ toJSON ok

yaml x = BS.putStr $ encodePretty defConfig $ toJSON x