{-# LANGUAGE OverloadedStrings #-}
module KernSpec.ASTSpec where

import           Data.Int
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Text.Parsec as P

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Kern.Core
import           Kern.AST
import           Kern.AST.Parser
import           Kern.AST.Pretty

instance Arbitrary Identifier where
  arbitrary = Ident <$> ident

ident :: Gen T.Text
ident = do
  idenH <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  idenT <- sublistOf(['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_")
  return $ T.pack (idenH:idenT)

instance Arbitrary Literal where
  arbitrary = oneof [ Int32Lit <$> arbitrary
                    , Int64Lit <$> arbitrary
                    , Float32Lit <$> arbitrary
                    , Float64Lit <$> arbitrary
                    , CharLit <$> arbitrary ]

instance Arbitrary Jump where
  arbitrary = oneof [ Goto <$> arbitrary
                    , return Continue
                    , return Break
                    , Return <$> arbitrary ]
  shrink (Return (Just e)) = let ss = shrink e
                                 f x = Return (Just x) in
                                 map f ss

instance Arbitrary Expr where
  arbitrary = oneof [ PrimC <$> arbitrary
                    , PrimI <$> arbitrary
                    , aMulExpr
                    , aAddExpr
                    , aShiftExpr
                    , aUnaryExpr
                    , aRelatExpr
                    , aEqualExpr
                    , aBwAndExpr
                    , aBwOrExpr
                    , aXorExpr
                    , aOrExpr
                    , aAddExpr
                    ]
  shrink (PrimC _) = []
  shrink (PrimI (Ident i)) = [PrimI (Ident (T.init i))]
  shrink (Neg e) = [e]
  shrink (Plus e) = [e]
  shrink (Not e) = [e]
  shrink (AddrOf e) = [e]
  shrink (Deref e) = [e]
  shrink (Compl e) = [e]
  shrink (PostfixInc e) = [e]
  shrink (PostfixDec e) = [e]
  shrink (PrefixInc e) = [e]
  shrink (PrefixDec e) = [e]
  shrink (MulExpr a b) = [a, b]
  shrink (DivExpr a b) = [a, b]
  shrink (ModExpr a b) = [a, b]
  shrink (AddExpr a b) = [a, b]
  shrink (SubExpr a b) = [a, b]
  shrink (ShiftL a b) = [a, b]
  shrink (ShiftR a b) = [a, b]
  shrink (Gt a b) = [a, b]
  shrink (Lt a b) = [a, b]
  shrink (LtEq a b) = [a, b]
  shrink (GtEq a b) = [a, b]
  shrink (NeqExpr a b) = [a, b]
  shrink (EqExpr a b) = [a, b]
  shrink (Xor a b) = [a, b]
  shrink (BitwiseAnd a b) = [a, b]
  shrink (BitwiseOr a b) = [a, b]
  shrink (And a b) = [a, b]
  shrink (Or a b) = [a, b]
  shrink (CondExpr a b c) = [a, b, c]
  shrink (Assign lh _ rh) = [lh, rh]

aOrExpr :: Gen Expr
aOrExpr = oneof [ aAndExpr
                 , Or <$> aAndExpr <*> aOrExpr ]

aAndExpr :: Gen Expr
aAndExpr = oneof [ aBwOrExpr
                 , And <$> aBwOrExpr <*> aAndExpr ]

aBwOrExpr :: Gen Expr
aBwOrExpr = oneof [ aXorExpr
                  , BitwiseOr <$> aXorExpr <*> aBwOrExpr ]

aXorExpr :: Gen Expr
aXorExpr = oneof [ aBwAndExpr
                 , Xor <$> aBwAndExpr <*> aXorExpr ]


aBwAndExpr :: Gen Expr
aBwAndExpr = oneof [ aEqualExpr
                   , BitwiseAnd <$> aEqualExpr <*> aBwAndExpr ]

aEqualExpr :: Gen Expr
aEqualExpr = oneof [ aRelatExpr
                   , EqExpr <$> aRelatExpr <*> aEqualExpr
                   , NeqExpr <$> aRelatExpr <*> aEqualExpr
                   ]

aRelatExpr :: Gen Expr
aRelatExpr = oneof [ aShiftExpr
                   , Lt <$> aShiftExpr <*> aRelatExpr
                   , Gt <$> aShiftExpr <*> aRelatExpr
                   , LtEq <$> aShiftExpr <*> aRelatExpr
                   , GtEq <$> aShiftExpr <*> aRelatExpr
                   ]

aShiftExpr :: Gen Expr
aShiftExpr = oneof [ aAddExpr
                   , ShiftL <$> aAddExpr <*> aShiftExpr
                   , ShiftR <$> aAddExpr <*> aShiftExpr
                   ]

aAddExpr :: Gen Expr
aAddExpr = oneof [ aMulExpr
                 , AddExpr <$> aMulExpr <*> aAddExpr
                 , SubExpr <$> aMulExpr <*> aAddExpr
                 ]

aMulExpr :: Gen Expr
aMulExpr = oneof [ aCastExpr
                 , MulExpr <$> aCastExpr <*> aMulExpr
                 , DivExpr <$> aCastExpr <*> aMulExpr
                 , ModExpr <$> aCastExpr <*> aMulExpr
                 ]

aCastExpr :: Gen Expr
aCastExpr = aPrimExpr --FIXME

aPrimExpr :: Gen Expr
aPrimExpr = oneof [ (PrimI <$> arbitrary)
                  , (PrimC <$> arbitrary) ]

aUnaryExpr :: Gen Expr
aUnaryExpr = oneof [ PostfixInc <$> aPrimExpr
                   , PostfixDec <$> aPrimExpr
                   , PrefixInc <$> aPrimExpr
                   , PrefixDec <$> aPrimExpr
                   , Neg <$> (PrimI <$> arbitrary)
                   , Plus <$> aCastExpr
                   , Not <$> aCastExpr
                   , AddrOf <$> aCastExpr
                   , Deref <$> aCastExpr
                   , Compl <$> aCastExpr
                   ]

instance Arbitrary Statement where
  arbitrary = oneof [ Jump <$> arbitrary
                    , ExprStmt <$> arbitrary ]
  shrink (ExprStmt Nothing) = []
  shrink (ExprStmt (Just e)) = let ss = shrink e
                                   f x = ExprStmt (Just x) in
                                   map f ss

prop_parse_pretty :: (Eq a, Pretty a) => (P.Parsec T.Text () a) -> a -> Bool
prop_parse_pretty p x =
  let ps = P.parse p "" (T.pack $ show $ pretty x) in
    ps == (Right x)

spec :: Spec
spec = do
  describe "identifier" $
    modifyMaxSuccess (const 100) $
    it "roundtrip" $ property (prop_parse_pretty identifier)
  describe "literals" $
    modifyMaxSuccess (const 250) $
    it "roundtrip" $ property (prop_parse_pretty literal)
  describe "jump statement" $
    modifyMaxSuccess (const 250) $
    it "roundtrip" $ property (prop_parse_pretty jump)
  describe "expressions" $
    modifyMaxSuccess (const 250) $
    it "roundtrip" $ property (prop_parse_pretty expr)
  describe "statements" $
    modifyMaxSuccess (const 250) $
    it "roundtrip" $ property (prop_parse_pretty statement)

