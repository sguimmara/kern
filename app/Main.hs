module Main where

import System.Environment
import System.Exit
import Data.Text           (pack, unpack)

import Compiler

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> putStrLn "usage: ccomp FILE" >> exitWith (ExitFailure 1)
        [path] -> do
            contents <- readFile path
            let result = compile $ pack contents
            case result of
                Left error -> putStrLn $ unpack error
                Right asm -> putStr $ unpack asm
