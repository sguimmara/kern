{-# LANGUAGE OverloadedStrings #-}

module Formatters.ATTAssembly
    ( format, printToStdout, emit
    , Listing (..), formatListing
    , Line (..), emitLine
    ) where

import CodeGen.Assembly

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text            (Text)

data Line = Line Bool Text
            deriving (Eq, Show)

emitLine :: Line -> Text
emitLine (Line indent txt) = if indent then T.cons '\t' txt else txt

type Listing = [Line]

printToStdout :: [Instr] -> IO ()
printToStdout = TIO.putStrLn . format

formatListing :: Listing -> Text
formatListing ls = T.unlines $ map emitLine ls

format :: [Instr] -> Text
format = formatListing . (map emit)

emit :: Instr -> Line
emit Ret                = Line True "ret"
emit (Rep Ret)          = Line True "rep ret"
emit (Label t)          = Line False (T.snoc t ':')