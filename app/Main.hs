{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment
import           System.Exit
import           Data.Text              (pack, unpack)
import qualified Data.Text.IO as TIO    (readFile, putStr, putStrLn)
import           System.FilePath        (takeFileName)

import Compiler


main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> TIO.putStrLn "usage: kern FILE" >> exitWith (ExitFailure 1)
        [path] -> do
            contents <- TIO.readFile path
            let result = process contents
            case result of
                Left error -> putStrLn $ show error
                Right asm -> TIO.putStr asm
