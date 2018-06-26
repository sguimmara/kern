{-# LANGUAGE OverloadedStrings #-}
module KernSpec.ASTSpec where

import           Data.Int
import           Data.List
import           Data.Functor.Identity
import qualified Data.Text as T

import           Text.PrettyPrint.HughesPJClass (Pretty, pPrint, render)
import qualified Text.Parsec as P

import           Control.Monad

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Kern.AST
import           Kern.AST.Analysis
import           Kern.AST.Parser
import           Kern.AST.Pretty

instance Arbitrary Identifier where
  arbitrary = Id <$> ident
  shrink (Id i) = [Id "_x_"]

ident :: Gen T.Text
ident = do
  idenH <- elements (['a'..'z'] ++ ['A'..'Z'] ++ "_")
  idenT <- sublistOf(['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z'] ++ "_")
  return $ T.pack (idenH:idenT)

instance Arbitrary DataType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary StorCls where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary TypeQual where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary InitDecltor where
  arbitrary = InitDecltor <$> arbitrary <*> (pure Nothing)
  shrink (InitDecltor d i) = [InitDecltor d' i | d' <- shrink d ]

instance Arbitrary Decltor where
  arbitrary = Decltor <$> arbitrary <*> arbitrary
  shrink (Decltor p d) = map (Decltor Nothing) (shrink d)

instance Arbitrary DirectDecltor where
  arbitrary = NameDecltor <$> arbitrary
  shrink (NameDecltor i) = map NameDecltor (shrink i)
  shrink (FuncDecltor i ps) =
    [FuncDecltor i' ps' | i' <- shrink i
                        , ps' <- shrink ps]

instance Arbitrary Pointer where
  arbitrary = do
    quals <- arbitrary
    p <- arbitrary
    return $ Pointer (sort $ nub quals) p
  shrink (Pointer qs p) = [Pointer qs' p' | qs' <- shrink qs
                                          , p' <- shrink p ]

instance Arbitrary Declaration where
  arbitrary = do
    dt <- arbitrary
    sc <- arbitrary
    quals <- arbitrary
    decls <- listOf1 arbitrary
    return (Declaration dt sc (sort $ nub quals) decls)
  shrink (Declaration dt sc qs ds) =
    [ Declaration dt' sc' (sort $ nub qs') ds' | dt' <- shrink dt
                                               , sc' <- shrink sc
                                               , qs' <- shrink qs
                                               , ds' <- shrink ds ]

-- instance Arbitrary Initializer where
--   arbitrary = InitExpr <$> arbitrary

instance Arbitrary Literal where
  arbitrary = oneof [ CInt32 <$> arbitrary
                    , CInt64 <$> arbitrary
                    , CFloat32 <$> arbitrary
                    , CFloat64 <$> arbitrary
                    , CChar <$> arbitrary ]

instance Arbitrary Statement where
  arbitrary =
    oneof
      [ ExprStmt <$> arbitrary
      , Return <$> arbitrary
      , Goto <$> arbitrary
      , return Break
      , return Continue
      , DefaultStmt <$> arbitrary
      , LabelStmt <$> arbitrary <*> arbitrary
      , Case <$> aCondExpr <*> arbitrary
      ]
  shrink s = case s of
    ExprStmt (Just e) -> map (\x -> (ExprStmt (Just x))) (shrink e)
    ExprStmt Nothing -> []
    Return Nothing -> []
    Return (Just e) -> map (\x -> (Return (Just x))) (shrink e)
    Goto i -> map Goto (shrink i)
    Break -> []
    Continue -> []
    Case e s -> let ss = shrink s
                    es = shrink e
                    in [Case e' s' | e' <- shrink e, s' <- shrink s ]
    DefaultStmt s -> map DefaultStmt (shrink s)
    LabelStmt i s -> [LabelStmt i' s' | i' <- shrink i, s' <- shrink s ]

instance Arbitrary Expr where
  arbitrary =
    oneof
      [ PrimC <$> arbitrary
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

  shrink e =
    case e of
    (PrimC _) -> []
    (PrimI i) -> map PrimI (shrink i)
    (Neg e) -> [e]
    (Plus e) -> [e]
    (Not e) -> [e]
    (AddrOf e) -> [e]
    (Deref e) -> [e]
    (Compl e) -> [e]
    (PostfixInc e) -> [e]
    (PostfixDec e) -> [e]
    (PrefixInc e) -> [e]
    (PrefixDec e) -> [e]
    (MulExpr a b) -> [a, b]
    (DivExpr a b) -> [a, b]
    (ModExpr a b) -> [a, b]
    (AddExpr a b) -> [a, b]
    (SubExpr a b) -> [a, b]
    (ShiftL a b) -> [a, b]
    (ShiftR a b) -> [a, b]
    (Gt a b) -> [a, b]
    (Lt a b) -> [a, b]
    (LtEq a b) -> [a, b]
    (GtEq a b) -> [a, b]
    (NeqExpr a b) -> [a, b]
    (EqExpr a b) -> [a, b]
    (Xor a b) -> [a, b]
    (BitwiseAnd a b) -> [a, b]
    (BitwiseOr a b) -> [a, b]
    (And a b) -> [a, b]
    (Or a b) -> [a, b]
    (CondExpr a b c) -> [a, b, c]
    (Assign lh _ rh) -> [lh, rh]

instance Arbitrary Op where
  arbitrary = arbitraryBoundedEnum

aCondExpr :: Gen Expr
aCondExpr =
  frequency [ (1, CondExpr <$> aOrExpr <*> arbitrary <*> aCondExpr)
            , (2, aOrExpr) ]

aOrExpr :: Gen Expr
aOrExpr =
  frequency [ (2, aAndExpr)
            , (1, Or <$> aAndExpr <*> aOrExpr) ]

aAndExpr :: Gen Expr
aAndExpr =
  frequency [ (2, aBwOrExpr)
            , (1, And <$> aBwOrExpr <*> aAndExpr) ]

aBwOrExpr :: Gen Expr
aBwOrExpr =
  frequency [ (2, aXorExpr)
            , (1, BitwiseOr <$> aXorExpr <*> aBwOrExpr) ]

aXorExpr :: Gen Expr
aXorExpr =
  frequency [ (2, aBwAndExpr)
            , (1, Xor <$> aBwAndExpr <*> aXorExpr) ]


aBwAndExpr :: Gen Expr
aBwAndExpr =
  frequency [ (2, aEqualExpr)
            , (1, BitwiseAnd <$> aEqualExpr <*> aBwAndExpr) ]

aEqualExpr :: Gen Expr
aEqualExpr =
  frequency [ (3, aRelatExpr)
            , (1, EqExpr <$> aRelatExpr <*> aEqualExpr)
            , (1, NeqExpr <$> aRelatExpr <*> aEqualExpr)
            ]

aRelatExpr :: Gen Expr
aRelatExpr =
  frequency [ (4, aShiftExpr)
            , (1, Lt <$> aShiftExpr <*> aRelatExpr)
            , (1, Gt <$> aShiftExpr <*> aRelatExpr)
            , (1, LtEq <$> aShiftExpr <*> aRelatExpr)
            , (1, GtEq <$> aShiftExpr <*> aRelatExpr)
            ]

aShiftExpr :: Gen Expr
aShiftExpr =
  frequency [ (3, aAddExpr)
            , (1, ShiftL <$> aAddExpr <*> aShiftExpr)
            , (1, ShiftR <$> aAddExpr <*> aShiftExpr)
            ]

aAddExpr :: Gen Expr
aAddExpr =
  frequency [ (3, aMulExpr)
            , (1, AddExpr <$> aMulExpr <*> aAddExpr)
            , (1, SubExpr <$> aMulExpr <*> aAddExpr)
            ]

aMulExpr :: Gen Expr
aMulExpr =
  frequency [ (4, aCastExpr)
            , (1, MulExpr <$> aCastExpr <*> aMulExpr)
            , (1, DivExpr <$> aCastExpr <*> aMulExpr)
            , (1, ModExpr <$> aCastExpr <*> aMulExpr)
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

qualifiers :: Gen [TypeQual]
qualifiers = do
  qs <- arbitrary
  return (sort $ nub qs)

instance Arbitrary FunctionDefinition where
  arbitrary =
    FuncDef <$> arbitrary <*> arbitrary <*> qualifiers <*> aFuncDecl <*> arbitrary
  shrink (FuncDef dt sc qs d cs) =
    [FuncDef dt' sc' (sort $ nub qs') d' cs' | dt' <- shrink dt
                                             , sc' <- shrink sc
                                             , qs' <- shrink qs
                                             , d' <- shrink d
                                             , cs' <- shrink cs ]

aFuncDecl :: Gen Decltor
aFuncDecl = do
  fd <- FuncDecltor <$> arbitrary <*> arbitrary
  p <- arbitrary
  return (Decltor p fd)

instance Arbitrary CompoundStatement where
  arbitrary = CompoundStmt <$> arbitrary <*> arbitrary
  shrink (CompoundStmt ds ss) =
    [CompoundStmt ds' ss' | ds' <- shrink ds
                          , ss' <- shrink ss ]

instance Arbitrary ParamDecl where
  arbitrary = ParamDecl <$> arbitrary <*>
                            arbitrary <*>
                            qualifiers <*>
                            (oneof [ Decltor <$> arbitrary <*> (NameDecltor <$> arbitrary) ])
  shrink (ParamDecl dt sc tq d) =
    [ParamDecl dt' sc' (sort $ nub tq') d' | dt' <- shrink dt
                                           , sc' <- shrink sc
                                           , tq' <- shrink tq
                                           , d' <- shrink d ]

-- instance Arbitrary Statement where
--   arbitrary = oneof [ Jump <$> arbitrary
--                     , ExprStmt <$> arbitrary ]
--   shrink (ExprStmt Nothing) = []
--   shrink (ExprStmt (Just e)) = let ss = shrink e
--                                    f x = ExprStmt (Just x) in
--                                    map f ss

-- instance Arbitrary Type where
--   arbitrary = Type <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary InternalType where
--   arbitrary =
--     InternalType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary ExternalType where
--   arbitrary =
--     ExternalType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary LocalVariable where
--   arbitrary = LocalVariable <$> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary GlobalVar where
--   arbitrary =
--    GlobalVar <$> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary ParameterType where
--   arbitrary =
--     ParameterType <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary Parameter where
--   arbitrary = Parameter <$> arbitrary <*> arbitrary

-- instance Arbitrary FunctionPrototype where
--   arbitrary = Prototype <$> arbitrary <*> arbitrary <*> arbitrary

-- instance Arbitrary Body where
--   arbitrary = Body <$> arbitrary <*> arbitrary

-- instance Arbitrary FunctionDefinition where
--   arbitrary = Function <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
--   shrink (Function ty i ps (Body ls ss)) =
--     [ Function ty i ps (Body (lhalf ls) (lhalf ss))
--     , Function ty i ps (Body (rhalf ls) (rhalf ss)) ]

-- lhalf :: [a] -> [a]
-- lhalf x = drop (length x `div` 2) x

-- rhalf :: [a] -> [a]
-- rhalf x = take (length x `div` 2) x

-- instance Arbitrary ExternalDeclaration where
--   arbitrary = oneof [ FunctionDefinition <$> arbitrary
--                     , GlobalDeclaration <$> arbitrary ]

-- instance Arbitrary TranslationUnit where
--   arbitrary = TranslationUnit <$> arbitrary
--   shrink (TranslationUnit ds) = [TranslationUnit [(head ds)], TranslationUnit (tail ds)]

prop_parse_pretty ::
  (Eq a, Pretty a) =>
  (P.Parsec T.Text ParserState a) -> a -> Bool
prop_parse_pretty p x = ps == Right x
  where ps = testParser p (T.pack $ render $ pPrint x)

spec :: Spec
spec = parallel $ do
  describe "identifiers" $
    it "roundtrip" $ property (prop_parse_pretty identifier)
  describe "literals" $
    it "roundtrip" $ property (prop_parse_pretty literal)
  describe "declarations" $
    it "roundtrip" $ property (prop_parse_pretty declaration)
  describe "expressions" $
    it "roundtrip" $ property (prop_parse_pretty expr)
  describe "statements" $
    it "roundtrip" $ property (prop_parse_pretty statement)
  describe "function definition" $
    it "roundtrip" $ property (prop_parse_pretty funcDef)
--   describe "expressions" $
--     it "roundtrip" $ property (prop_parse_pretty expr)
--   describe "statements" $
--     it "roundtrip" $ property (prop_parse_pretty statement)
--   describe "local variables" $
--     it "roundtrip" $ property (prop_parse_pretty localDeclaration)
--   describe "function prototypes" $
--     it "roundtrip" $ property (prop_parse_pretty functionPrototype)
--   describe "function definitions" $
--     modifyMaxSuccess (const 50) $
--     it "roundtrip" $ property (prop_parse_pretty functionDefinition)
