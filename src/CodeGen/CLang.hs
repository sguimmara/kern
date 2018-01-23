{-# LANGUAGE OverloadedStrings #-}

module CodeGen.CLang where

-- import           Core
-- import           AST
-- import           Prelude   hiding    (unwords, words)

-- import           Data.Text           (Text, cons, snoc, intercalate
--                                      , pack, unpack, words, unwords)

-- parens :: Text -> Text
-- parens t = snoc (cons '(' t) ')'

-- scoped :: Generable a => [a] -> Text
-- scoped xs = unwords $ map gen xs

-- paramlist :: [Parameter] -> Text
-- paramlist ps = parens $ intercalate "," $ map gen ps

-- class Generable a where
--   gen :: a -> Text

-- instance Generable Name where
--   gen (Name t) = t

-- instance Generable TypeSpec where
--   gen (TInt) = pack "int"
--   gen (TVoid) = pack "void"

-- instance Generable Local where
--   gen (Local t n) = unwords [gen t, gen n]

-- instance Generable Parameter where
--   gen (Param t n) = unwords [gen t, maybe "" gen n]

-- instance Generable Function where
--   gen (Function t n p l s) = unwords [ gen t, gen n, paramlist p ]
