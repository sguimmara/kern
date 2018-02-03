{-# LANGUAGE OverloadedStrings #-}

module Kern.Formatting.ATandT
    ( format, emit
    , Line (..), emitLine
    ) where

import Kern.Arch.X64

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Text            (Text)

data Line = Line Bool Text
            deriving (Eq, Show)

emitLine :: Line -> Text
emitLine (Line indent txt) = if indent then T.cons '\t' txt else txt

format :: [Instr] -> Text
format = T.unlines .(map emitLine) . (map emit)

line :: Text -> Line
line t = Line False t

ind :: Text -> Line
ind x = Line True x

emit :: Instr -> Line
emit Ret                = ind "ret"
emit (Rep Ret)          = ind "rep ret"
emit (Label t)          = line (T.snoc t ':')