{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           System.Exit
import qualified Data.ByteString as BS
import           Data.Yaml.Pretty
import qualified Text.Parsec     as P
import           Data.Yaml
import           Data.Text              (Text, pack, unpack)
import qualified Data.Text.IO as TIO    (readFile, putStr, putStrLn)
import           System.FilePath        (takeFileName)

import           Kern.AST
import           Kern.AST.Parser
import           Kern.Bytecode

main = undefined

-- main :: IO ()
-- main = do
--   args <- getArgs
--   case args of
--     ("--parse":path:[])    -> parseOnly path
--     ("--bytecode":path:[]) -> bytecodeOnly path
--     _                      -> printUsage

-- printUsage :: IO ()
-- printUsage = do
--     TIO.putStrLn "usage: kern [--parse | --bytecode] FILE"
--     exitWith (ExitFailure 1)

-- data StageError = ParseError P.ParseError deriving (Eq, Show)

-- parser :: FilePath -> IO (Either StageError TranslationUnit)
-- parser path = do
--   c <- TIO.readFile path
--   let res = P.parse translationUnit path c
--   case res of
--     (Left err) -> return (Left $ ParseError err)
--     (Right ok) -> return (Right ok)

-- parseOnly :: FilePath -> IO ()
-- parseOnly path = do
--   p <- parser path
--   case p of
--     Left err -> print err
--     Right ok -> yaml ok

-- bytecodeOnly :: FilePath -> IO ()
-- bytecodeOnly path = do
--   p <- parser path
--   case p of
--     Left err -> print err
--     Right ok -> yaml $ genTrUnit ok

-- yaml :: ToJSON a => a -> IO ()
-- yaml x = BS.putStr $ encodePretty defConfig $ toJSON x