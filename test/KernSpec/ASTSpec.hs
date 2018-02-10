{-# LANGUAGE OverloadedStrings #-}
module KernSpec.ASTSpec where

import           Data.Int
import           Data.Functor.Identity
import qualified Data.Text as T
import qualified Text.Parsec as P
import           Control.Monad

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

instance Arbitrary DataType where
  arbitrary = elements [ TyVoid, TyByte
                       , TyInt16, TyInt32, TyInt64
                       , TyUint16, TyUint32, TyUint64
                       , TyFloat32, TyFloat64
                       ]

instance Arbitrary Constness where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Volatility where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Linkage where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary Indirection where
  arbitrary = oneof [ return Direct
                    , Indirect <$> arbitrary ]

instance Arbitrary Pointer where
  arbitrary = oneof [ return (Pointer Nothing)
                    , Pointer <$> arbitrary ]

instance Arbitrary Initializer where
  arbitrary = InitExpr <$> arbitrary

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
                    , Assign <$> aUnaryExpr <*> arbitrary <*> arbitrary
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

instance Arbitrary Op where
  arbitrary = arbitraryBoundedEnum

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

instance Arbitrary Type where
  arbitrary = Type <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary InternalType where
  arbitrary =
    InternalType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ExternalType where
  arbitrary =
    ExternalType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary LocalVariable where
  arbitrary = LocalVariable <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary GlobalVar where
  arbitrary =
   GlobalVar <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary ParameterType where
  arbitrary =
    ParameterType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Parameter where
  arbitrary = Parameter <$> arbitrary <*> arbitrary

instance Arbitrary FunctionPrototype where
  arbitrary = Prototype <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Body where
  arbitrary = Body <$> arbitrary <*> arbitrary

instance Arbitrary FunctionDefinition where
  arbitrary = Function <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink (Function ty i ps (Body ls ss)) =
    [ Function ty i ps (Body (lhalf ls) (lhalf ss))
    , Function ty i ps (Body (rhalf ls) (rhalf ss)) ]

lhalf :: [a] -> [a]
lhalf x = drop (length x `div` 2) x

rhalf :: [a] -> [a]
rhalf x = take (length x `div` 2) x

instance Arbitrary ExternalDeclaration where
  arbitrary = oneof [ FunctionDefinition <$> arbitrary
                    , GlobalDeclaration <$> arbitrary ]

instance Arbitrary TranslationUnit where
  arbitrary = TranslationUnit <$> arbitrary
  shrink (TranslationUnit ds) = [TranslationUnit [(head ds)], TranslationUnit (tail ds)]

prop_parse_pretty :: (Eq a, Pretty a) => (P.Parsec T.Text () a) -> a -> Bool
prop_parse_pretty p x =
  let ps = P.parse p "" (T.pack $ show $ pretty x) in
    ps == (Right x)

spec :: Spec
spec = parallel $ do
  describe "identifiers" $
    it "roundtrip" $ property (prop_parse_pretty identifier)
  describe "literals" $
    it "roundtrip" $ property (prop_parse_pretty literal)
  describe "jump statements" $
    it "roundtrip" $ property (prop_parse_pretty jump)
  describe "expressions" $
    it "roundtrip" $ property (prop_parse_pretty expr)
  describe "statements" $
    it "roundtrip" $ property (prop_parse_pretty statement)
  describe "local variables" $
    it "roundtrip" $ property (prop_parse_pretty localDeclaration)
  describe "function prototypes" $
    it "roundtrip" $ property (prop_parse_pretty functionPrototype)
  describe "function definitions" $
    modifyMaxSuccess (const 50) $
    it "roundtrip" $ property (prop_parse_pretty functionDefinition)
