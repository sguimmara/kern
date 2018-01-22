{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment
import System.Exit
import Data.Text              (pack, unpack)
import Data.Text.IO as TIO    (readFile, putStr, putStrLn)
import System.FilePath        (takeFileName)

import Compiler

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> TIO.putStrLn "usage: ccomp FILE" >> exitWith (ExitFailure 1)
        [path] -> do
            contents <- TIO.readFile path
            let result = compile (takeFileName path) contents
            case result of
                Left error -> TIO.putStrLn error
                Right asm -> TIO.putStr asm
