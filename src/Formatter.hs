module Formatter where

-- module Formatter
--     ( format
--     , prettyPrint
--     ) where

-- import CodeGen

-- import Data.List       (intercalate)
-- import Data.Text       (Text, pack)

-- prettyPrint :: [Asm] -> Text
-- prettyPrint = pack . unlines . map format

-- format :: Asm -> String
-- -- Misc ----------------------------------------------------------------
-- format (Label s)           = s ++ ":"
-- format (Section s xs)      = fmt [dot s, lst xs]
-- -- Instructions---------------------------------------------------------
-- format Ret                 = fmt [ "ret" ]
-- format (Rep Ret)           = fmt [ "rep ret" ]
-- format (Mov sz op1 op2)    = fmt [ sized "mov" sz, lst $ map show [op1, op2] ]
-- format (Pop sz op1)        = fmt [ sized "pop" sz, lst $ map show [op1] ]
-- format (Push sz op1)       = fmt [ sized "push"sz, lst $ map show [op1] ]

-- sized :: String -> Size -> String
-- sized op size = op ++ show size

-- dot :: String -> String
-- dot s = "." ++ s

-- tab = "\t"

-- fmt []     = ""
-- fmt [x] = tab ++ x
-- fmt (x:xs) = tab ++ x ++ fmt xs

-- lst :: [String] -> String
-- lst = intercalate ", "
