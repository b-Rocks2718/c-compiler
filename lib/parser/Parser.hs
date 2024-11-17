module Parser where

import Utils
import Lexer
import ParserUtils
import AST

parseProg :: Parser Token Prog
parseProg = Prog <$> many parseDclr <* parseEOF

parseDclr :: Parser Token Declaration
parseDclr = FunDclr <$> parseFunction <|>
            VarDclr <$> parseVariableDclr

parseFunction :: Parser Token FunctionDclr
parseFunction = do
  (typeSpec, storageClass) <- parseTypeAndStorageClass
  name <- identName <$> satisfy isIdent
  params <- char OpenP *>
    ([] <$ char Void <* char CloseP <|> [] <$ char CloseP <|> some parseParam)
  let funType = FunType (vType <$> params) typeSpec
  FunctionDclr name funType storageClass params <$> parseEndOfFunc

parseEndOfFunc :: Parser Token (Maybe Block)
parseEndOfFunc = do
  body <- optional parseBlock
  case body of
    Just _ -> return body -- function definition
    Nothing -> Nothing <$ char Semi -- function declaration

parseParam :: Parser Token VariableDclr
parseParam = do
  type_ <- parseParamType
  name <- (identName <$> satisfy isIdent) <* (char Comma <|> char CloseP)
  return (VariableDclr name type_ Nothing Nothing)

parseTypeSpec :: Parser Token TypeSpecifier
parseTypeSpec = IntSpec <$ char IntTok <|>
                UIntSpec <$ char UnsignedTok <|>
                SIntSpec <$ char SignedTok

-- ensures the list of type specifiers makes sense
parseParamType :: Parser Token Type_
parseParamType = do
  types <- some parseTypeSpec
  if hasDuplicates types then
    errorParse $ "Duplicate type specifiers: " ++ show types
  else if SIntSpec `elem` types && UIntSpec `elem` types then
    errorParse $ "Invalid type specifiers: " ++ show types
  else if UIntSpec `elem` types then
    return UIntType
  else
    return IntType

parseBlock :: Parser Token Block
parseBlock = char OpenB *> (Block <$> many parseBlockItem) <* char CloseB

parseBlockItem :: Parser Token BlockItem
parseBlockItem =
  StmtBlock <$> parseStmt <|>
  DclrBlock <$> parseDclr

parseStmt :: Parser Token Stmt
parseStmt =
  liftA2 RetStmt (char ReturnTok *> parseExpr <* char Semi) (pure Nothing) <|>
  ExprStmt <$> (parseExpr <* char Semi) <|>
  liftA3 IfStmt
    (char IfTok *> char OpenP *> parseExpr <* char CloseP)
    parseStmt (optional (char ElseTok *> parseStmt)) <|>
  liftA2 LabeledStmt (identName <$> satisfy isIdent <* char Colon) parseStmt <|>
  GoToStmt . identName <$> (char GoToTok *> satisfy isIdent <* char Semi) <|>
  CompoundStmt <$> parseBlock <|>
  BreakStmt Nothing <$ (char BreakTok <* char Semi) <|>
  ContinueStmt Nothing <$ (char ContinueTok <* char Semi) <|>
  char WhileTok *> liftA3 WhileStmt (char OpenP *> parseExpr <* char CloseP)
    parseStmt (pure Nothing) <|>
  char DoTok *> liftA3 DoWhileStmt parseStmt
    (char WhileTok *> char OpenP *> parseExpr <* char CloseP <* char Semi) (pure Nothing) <|>
  char ForTok *> lift5 ForStmt
    (char OpenP *> parseForInit)
    (optional parseExpr <* char Semi)
    (optional parseExpr <* char CloseP)
    parseStmt (pure Nothing) <|>
  char SwitchTok *> lift4 SwitchStmt (char OpenP *> parseExpr <* char CloseP) 
    parseStmt (pure Nothing) (pure Nothing) <|>
  char CaseTok *> liftA3 CaseStmt (parseExpr <* char Colon) parseStmt (pure Nothing) <|>
  char DefaultTok *> char Colon *> liftA2 DefaultStmt parseStmt (pure Nothing) <|>
  NullStmt <$ char Semi

parseForInit :: Parser Token ForInit
parseForInit =
  InitDclr <$> parseForDclr <|>
  InitExpr <$> optional parseExpr <* char Semi

parseForDclr :: Parser Token VariableDclr
parseForDclr = do
  type_ <- typeSpecToType <$> parseTypeSpec
  name <- identName <$> satisfy isIdent
  expr <- optional (char Equals *> parseExpr) <* char Semi
  return (VariableDclr name type_ Nothing expr)

typeSpecToType :: TypeSpecifier -> Type_
typeSpecToType spec = case spec of
  IntSpec -> IntType
  SIntSpec -> IntType
  UIntSpec -> UIntType

parseVariableDclr :: Parser Token VariableDclr
parseVariableDclr = do
  (type_, storageClass) <- parseTypeAndStorageClass
  name <- identName <$> satisfy isIdent
  expr <- optional (char Equals *> parseExpr) <* char Semi
  return (VariableDclr name type_ storageClass expr)

-- parses a type specifier or a storage class
parseTypeOrStorageClass :: Parser Token DclrPrefix
parseTypeOrStorageClass =
  TypePrefix IntSpec <$ char IntTok <|>
  TypePrefix UIntSpec <$ char UnsignedTok <|>
  TypePrefix SIntSpec <$ char SignedTok <|>
  StoragePrefix Static <$ char StaticTok <|>
  StoragePrefix Extern <$ char ExternTok

-- type and storage specifiers can come in any order
-- this function separates them into two lists
parseTypesAndStorageClasses :: Parser Token ([TypeSpecifier], [StorageClass])
parseTypesAndStorageClasses = do
  prefixes <- many parseTypeOrStorageClass
  let f prefix (types, storageClasses) = (case prefix of
        TypePrefix t -> (t : types, storageClasses)
        StoragePrefix s-> (types, s : storageClasses))
  return (foldr f ([], []) prefixes)

-- ensures the list of types and storage specifiers makes sense
parseTypeAndStorageClass :: Parser Token (Type_, Maybe StorageClass)
parseTypeAndStorageClass = do
  (types, storageClasses) <- parseTypesAndStorageClasses
  if hasDuplicates types then
    errorParse $ "Duplicate type specifiers: " ++ show types
  else if null types then
    failParse
  else if SIntSpec `elem` types && UIntSpec `elem` types then
    errorParse $ "Invalid type specifiers: " ++ show types
  else if length storageClasses > 1 then
    errorParse $ "Invalid storage class: " ++ show storageClasses
  else if UIntSpec `elem` types then
    return (UIntType, safeHead storageClasses)
  else
    return (IntType, safeHead storageClasses)

parseIntLit :: Parser Token Expr
parseIntLit = do
  n <- intLitVal <$> satisfy isIntLit
  -- TODO:
  --if n >= 2 ^ (15 :: Int) then
  --  errorParse "Constant token is too large to represent as an int"
  --else
  return (Lit (ConstInt n))

parseUIntLit :: Parser Token Expr
parseUIntLit = do
  n <- intLitVal <$> satisfy isUIntLit
  -- TODO:
  --if n >= 2 ^ (16 :: Int) then
  --  errorParse "Constant token is too large to represent as an unsigned int"
  --else
  return (Lit (ConstInt n))

parseUnary :: Parser Token Expr
parseUnary = liftA2 Unary
              (getUnaryOp <$> satisfy isUnaryOp)
              parseFactor <|>
             parsePreIncDec

parsePreIncDec :: Parser Token Expr
parsePreIncDec = do
  op <- char IncTok <|> char DecTok
  v <- parseParenVar
  let binop = if op == IncTok then PlusEqOp else MinusEqOp
  -- rewrite '++v' as 'v += 1'
  return (Binary binop v (Lit (ConstInt 1)))

-- adds next factor to binary expression
-- needed to make operators left-associative
parseBinExpand :: Parser Token Expr -> Int -> Parser Token (Maybe Expr)
parseBinExpand parser minPrec =
  expanded <|> pure Nothing where
  expanded = do
    left <- parser
    op <- tokenToBinOp <$> satisfy isBinOp
    let nextPrec = getPrec op
    if nextPrec >= minPrec
    then do
        -- higher precedence: create new ASTExpr
        if op == AssignOp then do
          right <- parseBin parseFactor nextPrec
          return . Just $ Assign left right
        else if op `elem` compoundOps then do
          right <- parseBin parseFactor nextPrec
          return . Just $ Binary op left right
        else if op == TernaryOp then do
          middle <- parseExpr <* char Colon
          right <- parseBin parseFactor nextPrec
          return . Just $ Conditional left middle right
        else do
          right <- parseBin parseFactor (nextPrec + 1)
          return . Just $ Binary op left right
    else empty -- will just use left expr

-- need monadic parsing to avoid left-recursion
parseBin :: Parser Token Expr -> Int -> Parser Token Expr
parseBin parser minPrec = do
  mEnd <- parseBinExpand parser minPrec
  case mEnd of
    Nothing -> parser
    Just newLeft -> parseBin (pure newLeft) minPrec

parseParens :: Parser Token Expr
parseParens = char OpenP *> parseExpr <* char CloseP

parseFactor :: Parser Token Expr
parseFactor = parseIntLit <|>
              parseUIntLit <|>
              parseCast <|>
              parseUnary <|>
              parsePostIncDec <|>
              parseParens <|>
              liftA2 FunctionCall (identName <$> satisfy isIdent)
                (char OpenP *> ([] <$ char CloseP <|> some parseArg)) <|>
              parseVar

-- cast / param types are similar in that storage specifiers aren't allowed
parseCast :: Parser Token Expr
parseCast = liftA2 Cast
  (char OpenP *> parseParamType <* char CloseP)
  parseExpr

parseArg :: Parser Token Expr
parseArg = parseExpr <*
  (char Comma <|> char CloseP)

parseVar :: Parser Token Expr
parseVar = Var . identName <$> satisfy isIdent

parseParenVar :: Parser Token Expr
parseParenVar = parseVar <|> (char OpenP *> parseParenVar <* char CloseP)

parsePostIncDec :: Parser Token Expr
parsePostIncDec = do
  v <- parseParenVar
  op <- char IncTok <|> char DecTok
  let postOp = if op == IncTok then PostInc else PostDec
  return (PostAssign v postOp)

parseExpr :: Parser Token Expr
parseExpr = parseBin parseFactor 0

-- to ensure the entire file was parsed
parseEOF :: Parser Token ()
parseEOF = Parser f
  where
    f ts
      | null ts = Ok ((), ts)
      | otherwise = Err $ "Syntax Error: Could not parse " ++ show ts

parseDebug :: Parser Token a
parseDebug = Parser f
  where
    f ts = error $ "Debug: " ++ show ts