module KernSpec.ASTSpec where

import           Data.Int
import qualified Data.Text as T
import qualified Text.Parsec as P

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Kern.Core
import           Kern.AST
import           Kern.AST.Parser

instance Arbitrary Identifier where
  arbitrary = Ident <$> ident

prop_Identifier :: Identifier -> Bool
prop_Identifier i@(Ident t) =
  let res = P.parse identifier "" t in
    case res of
      Right ok -> ok == i
      Left  _  -> False

instance Arbitrary Literal where
  arbitrary = oneof [ Int32Lit <$> arbitrary ]

prop_Literal :: Literal -> Bool
prop_Literal l = P.parse literal "" (pr)

ident :: Gen T.Text
ident = do
  idenH <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  idenT <- sublistOf(['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_")
  return $ T.pack (idenH:idenT)

spec :: Spec
spec = do
  describe "identifier" $
    it "roundtrip" $ property prop_Identifier
  describe "literals" $
    it "roundtrip" $ property prop_Literal
