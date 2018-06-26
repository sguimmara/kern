module Kern.AST.Parser.Declarations where

import           Data.List
import           Data.Maybe

import           Text.Parsec
import           Text.Parsec.Char
import           Text.Parsec.Text

import           Kern.AST
import           Kern.AST.Analysis
import           Kern.AST.Parser.Utils
import           Kern.AST.Parser.Expressions
import           Kern.AST.Parser.Statements

data SpecQual
  = S TypeSpec
  | Q TypeQual
  deriving (Eq, Show)

data StoSpecQual
  = TS TypeSpec
  | TQ TypeQual
  | SC StorCls
  deriving (Eq, Show)

storCls :: GenParser ParserState StorCls
storCls =
  choice [ try $ keyword "auto" >> return Auto
         , try $ keyword "register" >> return Register
         , try $ keyword "static" >> return Static
         , try $ keyword "extern" >> return Extern
         , try $ keyword "typedef" >> return Typedef ]

typeQual :: GenParser ParserState TypeQual
typeQual =
  (try $ (keyword "const" >> return Const)) <|>
  (try $ (keyword "volatile" >> return Volatile))

typeSpec :: GenParser ParserState TypeSpec
typeSpec =
  choice
    [ try $ keyword "void" >> return SpecVoid
    , try $ keyword "char" >> return SpecChar
    , try $ keyword "int" >> return SpecInt
    , try $ keyword "long" >> return SpecLong
    , try $ keyword "short" >> return SpecShort
    , try $ keyword "float" >> return SpecFloat
    , try $ keyword "double" >> return SpecDouble
    , try $ keyword "signed" >> return SpecSigned
    , try $ keyword "unsigned" >> return SpecUnsigned
    , try $ TypedefId <$> existingIdentifier
    , try structOrUnion
    ]

structOrUnion :: GenParser ParserState TypeSpec
structOrUnion = do
  kw <- keyword "struct" <|> keyword "union"
  i <- optionMaybe newIdentifier
  b <- optionMaybe $ between lbrace rbrace (many1 structDecl)
  case kw of
    "struct" -> return $ Struct i (fromMaybe [] b)
    "union"  -> return $ Union i (fromMaybe [] b)

structDecl :: GenParser ParserState StructDecl
structDecl = do
  qs <- many ((Q <$> typeQual) <|> (S <$> typeSpec))
  let (q, s) = partition (\x -> case x of
                                  Q _ -> True
                                  S _ -> False) qs
  let f (S s) = s
  let g (Q q) = q
  let quals = map g (nub q)
  let specs = map f s
  let edt = dataType specs
  case edt of
    Left e -> fail e
    Right dt -> do
      dcls <- structDecltor `sepBy1` comma
      semi
      return $ StructDecl dt quals dcls

structDecltor :: GenParser ParserState StructDecltor
structDecltor = do
  d <- decltor
  bf <- optionMaybe colon
  case bf of
    Just _ -> unexpected $ unsupported "struct bitfields"
    Nothing -> return $ StructDecltor d

decltor :: GenParser ParserState Decltor
decltor = Decltor <$> (optionMaybe pointer) <*> directDecltor

pointer :: GenParser ParserState Pointer
pointer =
  star >> Pointer <$> many typeQual <*> optionMaybe pointer

directDecltor :: GenParser ParserState DirectDecltor
directDecltor =
  choice
    [ try funcDecltor
    , parenDecltor
    , otherDecltors
    , NameDecltor <$> newIdentifier
    ]

funcDecltor :: GenParser ParserState DirectDecltor
funcDecltor = do
  i <- identifier
  ps <- between lparen rparen (paramDecl `sepBy` (char ',' >> spaces))
  return $ FuncDecltor i ps

parenDecltor :: GenParser ParserState DirectDecltor
parenDecltor = between lparen rparen directDecltor

otherDecltors :: GenParser ParserState DirectDecltor
otherDecltors = do
  i <- identifier
  x <- optionMaybe (char '[')
  case x of
    Nothing -> return (NameDecltor i)
    Just _  -> do
      e <- optionMaybe condExpr
      char ']'
      return (ArrayDecltor i e)

paramDecl :: GenParser ParserState ParamDecl
paramDecl = do
  (dt, sc, tqs) <- declType
  d <- decltor
  return (ParamDecl dt sc tqs d)

declType :: GenParser ParserState (DataType, StorCls, [TypeQual])
declType = do
  ds <- many $
    (TS <$> typeSpec)
    <|> (TQ <$> typeQual)
    <|> (try $ SC <$> storCls)
  let sc = mapMaybe (\x -> case x of
                      SC s -> Just s
                      _    -> Nothing) ds
  let ts = mapMaybe (\x -> case x of
                      TS s -> Just s
                      _    -> Nothing) ds
  let tq = mapMaybe (\x -> case x of
                      TQ s -> Just s
                      _    -> Nothing) ds
  if (length sc) > 1
    then fail "multiple storage classes"
    else do
      let edt = dataType ts
      case edt of
        Left e -> fail e
        Right dt ->
          return (dt, if null sc then DefaultStorage else (head sc), nub tq)

enum :: GenParser ParserState TypeSpec
enum = do
  keyword "enum"
  i <- optionMaybe newIdentifier
  es <- between lbrace rbrace (enumerator `sepBy1` comma)
  return (SpecEnum i es)

enumerator :: GenParser ParserState Enumerator
enumerator = do
  i <- newIdentifier
  x <- optionMaybe (char '=')
  case x of
    Just _ -> fail $ unsupported "value assignments in enums"
    Nothing -> return (Enumerator i)

initDecltor :: GenParser ParserState InitDecltor
initDecltor =
  InitDecltor <$> decltor <*> (optionMaybe initializer)

initializer :: GenParser ParserState Initializer
initializer = equal >> (fail $ unsupported "initializers")

declaration :: GenParser ParserState Declaration
declaration = do
  (dt, sc, tqs) <- declType
  decls <- initDecltor `sepBy` comma
  semi
  if null decls
    then fail "useless type name in empty declaration"
    else return $ Declaration dt sc tqs decls

compound :: GenParser ParserState CompoundStatement
compound = between lbrace rbrace
  (CompoundStmt <$> (many (try declaration)) <*> (many statement))

funcDef :: GenParser ParserState FunctionDefinition
funcDef = do
  (dt, sc, tqs) <- declType
  d <- Decltor <$> (optionMaybe pointer) <*> funcDecltor
  unsuppDecl <- many declaration
  if (not . null) unsuppDecl
    then fail $ unsupported "old style declaration"
    else do
      cs <- compound
      return $ FuncDef dt sc tqs d cs