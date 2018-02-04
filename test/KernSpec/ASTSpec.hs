module KernSpec.ASTSpec where

import qualified Data.Text as T
import qualified Text.Parsec as P

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Kern.AST
import           Kern.AST.Parser

instance Arbitrary Identifier where
  arbitrary = Ident <$> ident

prop_parseIdentifier :: Identifier -> Bool
prop_parseIdentifier i@(Ident t) =
  let res = P.parse identifier "" t in
    case res of
      Right ok -> ok == i
      Left  _  -> False

ident :: Gen T.Text
ident = do
  idenH <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  idenT <- sublistOf(['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_")
  return $ T.pack (idenH:idenT)

spec :: Spec
spec = do
  describe "identifier" $
   it "roundtrip" $ property (prop_parseIdentifier :: Identifier -> Bool)
