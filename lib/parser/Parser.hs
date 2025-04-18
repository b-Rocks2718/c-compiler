module Parser where

import Utils
import Lexer
import ParserUtils
import AST

-- program is just a list of declarations
parseProg :: Parser Token Prog
parseProg = Prog <$> many parseDclr <* parseEOF

parseDclr :: Parser Token Declaration
parseDclr = do
  -- base type and storage specifiers are the first part of a declaration
  (baseType, mStorage) <- parseTypeAndStorageClass
  -- next is the declarator
  declarator <- parseDeclarator
  -- base type and declarator must be parsed to work out the derived type
  (name, declType, params) <-
    case processDeclarator declarator baseType of
      Ok x -> return x
      Err msg -> errorParse msg
      Fail -> failParse
  -- the rest of the declaration is either a function body or expression
  case declType of
    FunType _ retType -> FunDclr <$> parseFunction retType mStorage name params
    _ -> VarDclr <$> parseVariableDclr declType mStorage name

parseDeclarator :: Parser Token Declarator
parseDeclarator =
  char Asterisk *> (PointerDec <$> parseDeclarator) <|> -- pointer
  parseDirectDeclarator

parseDirectDeclarator :: Parser Token Declarator
parseDirectDeclarator = do
  d <- parseSimpleDeclarator
  suffix <- parseDeclaratorSuffix
  case suffix of
    Just (Left params) -> return $ FunDec params d
    Just (Right n) -> return $ makeArrayDec d n
    Nothing -> return d

makeArrayDec :: Declarator -> [Int] -> Declarator
makeArrayDec d sizes = 
  case sizes of
    n:ns -> ArrayDec (makeArrayDec d ns) n
    [] -> d

parseDeclaratorSuffix :: Parser Token (Maybe (Either [ParamInfo] [Int]))
parseDeclaratorSuffix = do
  ps <- optional parseParams
  sizes <- many (intLitVal <$> (char OpenS *> satisfy isIntLit <* char CloseS))
  case ps of 
    Just params -> return $ Just $ Left params
    Nothing -> case sizes of
      _:_ -> return $ Just $ Right sizes
      [] -> return Nothing

parseSimpleDeclarator :: Parser Token Declarator
parseSimpleDeclarator =
  -- function or variable name
  IdentDec . identName <$> satisfy isIdent <|>
  -- declarator can be enclosed in parenthesis
  char OpenP *> parseDeclarator <* char CloseP

parseParams :: Parser Token [ParamInfo]
parseParams = char OpenP *>
    ([] <$ char Void <* char CloseP <|> -- parses f(void) -- parses f(void)
     -- parses f(void)
    [] <$ char CloseP <|>               -- parses f()
    some parseParam)                    -- parses f([params]) 

parseParam :: Parser Token ParamInfo
parseParam = do
  type_ <- parseParamType
  declarator <- parseDeclarator <* (char Comma <|> char CloseP)
  return (Param type_ declarator)

-- converts declarators and base types into actual types
processDeclarator :: Declarator -> Type_ -> Result (String, Type_, [VariableDclr])
processDeclarator decl baseType = case decl of
  IdentDec name -> Ok (name, baseType, [])
  PointerDec d -> processDeclarator d (PointerType baseType)
  FunDec params d -> case d of
    IdentDec name -> do
      (paramNames, types) <- unzip <$> processParamsInfo params
      let derivedType = FunType types baseType
      -- convert list of names and types to list of 'VariableDclr's
      let dclrs = getZipList $ (\n t -> VariableDclr n t Nothing Nothing)
                  <$> ZipList paramNames <*> ZipList types
      return (name, derivedType, dclrs)
    _ -> Err "Parse Error: Can't apply additional type derivations to a function type"
  ArrayDec inner size ->
    let derived = ArrayType baseType size
    in  processDeclarator inner derived

processParamInfo :: ParamInfo -> Result (String, Type_)
processParamInfo (Param type_ d) = do
  case d of
    FunDec _ _ -> Err "Parse Error: Function pointers in parameters aren't supported"
    _ -> return ()
  (paramName, paramType_, _) <- processDeclarator d type_
  return (paramName, paramType_)

-- converts declarators and base types into actual types, 
-- but for function parameters
processParamsInfo :: [ParamInfo] -> Result [(String, Type_)]
processParamsInfo =
  foldr (\p ps -> case processParamInfo p of
  Ok p' -> liftA2 (:) (Ok p') ps
  Err s -> Err s
  Fail -> Fail) (Ok [])

parseFunction :: Type_ -> Maybe StorageClass -> String -> [VariableDclr] ->
    Parser Token FunctionDclr
parseFunction type_ mStorage name params = do
  let funType = FunType (vType <$> params) type_
  FunctionDclr name funType mStorage params <$> parseEndOfFunc

parseEndOfFunc :: Parser Token (Maybe Block)
parseEndOfFunc = do
  body <- optional parseBlock
  case body of
    Just _ -> return body -- function definition
    Nothing -> Nothing <$ char Semi -- function declaration

parseTypeSpec :: Parser Token TypeSpecifier
parseTypeSpec = IntSpec <$ char IntTok <|>
                UIntSpec <$ char UnsignedTok <|>
                SIntSpec <$ char SignedTok

-- ensures the list of type specifiers makes sense
parseParamType :: Parser Token Type_
parseParamType = do
  types <- some parseTypeSpec
  if hasDuplicates types then
    errorParse $ "Parse Error: Duplicate type specifiers - " ++ show types
  else if SIntSpec `elem` types && UIntSpec `elem` types then
    errorParse $ "Parser Error: Invalid type specifiers - " ++ show types
  else if UIntSpec `elem` types then
    return UIntType
  else
    return IntType

-- a block is a compound statement or function body
parseBlock :: Parser Token Block
parseBlock = char OpenB *> (Block <$> many parseBlockItem) <* char CloseB

-- block items are either statements or declarations
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

-- a ForInit could be a declaration or expression
parseForInit :: Parser Token ForInit
parseForInit =
  InitDclr <$> parseForDclr <|>
  InitExpr <$> optional parseExpr <* char Semi

parseForDclr :: Parser Token VariableDclr
parseForDclr = do
  type_ <- parseParamType
  declarator <- parseDeclarator
  init_ <- optional (char Equals *> parseInit) <* char Semi
  case processParamInfo (Param type_ declarator) of
    Ok (name, newType) -> return (VariableDclr name newType Nothing init_)
    Err msg -> errorParse msg
    Fail -> failParse

typeSpecToType :: TypeSpecifier -> Type_
typeSpecToType spec = case spec of
  IntSpec -> IntType
  SIntSpec -> IntType
  UIntSpec -> UIntType
  LongSpec -> LongType

-- initializing expression is optional for variable declaration
parseVariableDclr :: Type_ -> Maybe StorageClass -> String -> Parser Token VariableDclr
parseVariableDclr type_ mStorage name = do
  init_ <- optional (char Equals *> parseInit) <* char Semi
  return (VariableDclr name type_ mStorage init_)

-- parses a type specifier or a storage class
parseTypeOrStorageClass :: Parser Token DclrPrefix
parseTypeOrStorageClass =
  TypePrefix IntSpec <$ char IntTok <|>
  TypePrefix UIntSpec <$ char UnsignedTok <|>
  TypePrefix SIntSpec <$ char SignedTok <|>
  TypePrefix LongSpec <$ char LongTok <|>
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
    errorParse $ "Parse Error: Duplicate type specifiers - " ++ show types
  else if null types then
    failParse
  else if SIntSpec `elem` types && UIntSpec `elem` types then
    errorParse $ "Parse Error: Invalid type specifiers - " ++ show types
  else if length storageClasses > 1 then
    errorParse $ "Parse Error: Invalid storage class - " ++ show storageClasses
  else if UIntSpec `elem` types && LongSpec `elem` types then
    return (ULongType, safeHead storageClasses)
  else if UIntSpec `elem` types then
    return (UIntType, safeHead storageClasses)
  else if LongSpec `elem` types then
    return (LongType, safeHead storageClasses)
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

parseLongLit :: Parser Token Expr
parseLongLit = do
  n <- intLitVal <$> satisfy isLongLit
  -- TODO:
  --if n >= 2 ^ (31 :: Int) then
  --  errorParse "Constant token is too large to represent as an int"
  --else
  return (Lit (ConstLong n))

parseUIntLit :: Parser Token Expr
parseUIntLit = do
  n <- intLitVal <$> satisfy isUIntLit
  -- TODO:
  --if n >= 2 ^ (16 :: Int) then
  --  errorParse "Constant token is too large to represent as an unsigned int"
  --else
  return (Lit (ConstUInt n))

parseULongLit :: Parser Token Expr
parseULongLit = do
  n <- intLitVal <$> satisfy isULongLit
  -- TODO:
  --if n >= 2 ^ (32 :: Int) then
  --  errorParse "Constant token is too large to represent as an int"
  --else
  return (Lit (ConstULong n))

parseUnary :: Parser Token Expr
parseUnary = liftA2 Unary
              (getUnaryOp <$> satisfy isUnaryOp)
              parseFactor <|>
             parsePreIncDec <|>
             char Ampersand *> (AddrOf <$> parseExpr) <|>
             char Asterisk *> (Dereference <$> (parseUnary <|> parseVar <|> parseFactor))

-- parses ++v or --v
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
parseFactor = parseUnary <|>
              parsePostIncDec <|>
              parseCast <|>
              parsePostfix

parsePostfix :: Parser Token Expr
parsePostfix = do
  expr <- parsePrimaryExpr
  subscripts <- many (char OpenS *> parseExpr <* char CloseS)
  return (makeSubscriptExpr expr subscripts)

makeSubscriptExpr :: Expr -> [Expr] -> Expr
makeSubscriptExpr expr subscripts = 
  case subscripts of
    x:xs -> makeSubscriptExpr (Subscript expr x) xs
    [] -> expr

parsePrimaryExpr :: Parser Token Expr
parsePrimaryExpr = parseIntLit <|>
                   parseLongLit <|>
                   parseUIntLit <|>
                   parseULongLit <|>
                   parseParens <|>
                   liftA2 FunctionCall (identName <$> satisfy isIdent)
                    (char OpenP *> ([] <$ char CloseP <|> some parseArg)) <|>
                   parseVar

-- cast / param types are similar in that storage specifiers aren't allowed
parseCast :: Parser Token Expr
parseCast = do
  _ <- char OpenP
  baseType <- parseParamType
  declarator <- parseAbstractDeclarator
  _ <- char CloseP
  expr <- parseExpr
  let derivedType = processAbstractDeclarator declarator baseType
  return (Cast derivedType expr)

-- converts abstract declarators and base types into actual types
-- used for casts to pointer types
processAbstractDeclarator :: AbstractDeclarator -> Type_ -> Type_
processAbstractDeclarator decl baseType = case decl of
  AbstractBase -> baseType
  AbstractPointer d -> processAbstractDeclarator d (PointerType baseType)
  AbstractArray inner size ->
    let derived = ArrayType baseType size
    in  processAbstractDeclarator inner derived

parseAbstractDeclarator :: Parser Token AbstractDeclarator
parseAbstractDeclarator = 
  (char Asterisk *> (AbstractPointer <$> parseAbstractDeclarator)) <|>
  parseDirectAbstractDeclarator <|>
  pure AbstractBase

parseDirectAbstractDeclarator :: Parser Token AbstractDeclarator
parseDirectAbstractDeclarator = (do
  _ <- char OpenP
  d <- parseAbstractDeclarator
  _ <- char CloseP
  sizes <- many (intLitVal <$> (char OpenS *> satisfy isIntLit <* char CloseS))
  return (makeAbstractArrayDeclarator d sizes)) <|> (do
  sizes <- some (intLitVal <$> (char OpenS *> satisfy isIntLit <* char CloseS))
  return (makeAbstractArrayDeclarator AbstractBase sizes))

makeAbstractArrayDeclarator :: AbstractDeclarator -> [Int] -> AbstractDeclarator
makeAbstractArrayDeclarator d sizes = 
  case sizes of
    n:ns -> AbstractArray (makeAbstractArrayDeclarator d ns) n
    [] -> d

parseArg :: Parser Token Expr
parseArg = parseExpr <* (char Comma <|> char CloseP)

parseVar :: Parser Token Expr
parseVar = Var . identName <$> satisfy isIdent

parseParenVar :: Parser Token Expr
parseParenVar = parseVar <|> (char OpenP *> parseParenVar <* char CloseP)

-- parses v++ or v--
parsePostIncDec :: Parser Token Expr
parsePostIncDec = do
  v <- parseParenVar
  op <- char IncTok <|> char DecTok
  let postOp = if op == IncTok then PostInc else PostDec
  return (PostAssign v postOp)

parseExpr :: Parser Token Expr
parseExpr = parseBin parseFactor 0

parseInit :: Parser Token VarInit
parseInit = (SingleInit <$> parseExpr) <|>
            parseCompountInit

parseCompountInit :: Parser Token VarInit
parseCompountInit = do
  _ <- char OpenB
  firstInit <- parseInit
  otherInits <- many (char Comma *> parseInit)
  _ <- optional (char Comma)
  _ <- char CloseB
  return (CompoundInit $ firstInit : otherInits)

-- to ensure the entire file was parsed
parseEOF :: Parser Token ()
parseEOF = Parser f
  where
    f ts
      | null ts = Ok ((), ts)
      | otherwise = Err $ "Parse Error: Could not parse " ++ show ts

-- for debugging
parseDebug :: Parser Token a
parseDebug = Parser f
  where
    f ts = error $ "Debug: " ++ show ts