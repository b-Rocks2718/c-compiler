module Parser where

import Lexer
import Control.Applicative
import Control.Applicative.HT
import Data.List ( intercalate )

-- AST data structure
newtype ASTProg = ASTProg [Declaration]

data BlockItem = StmtBlock ASTStmt | DclrBlock Declaration

newtype Block = Block [BlockItem]

data ASTStmt = RetStmt ASTExpr
             | ExprStmt ASTExpr
             | IfStmt ASTExpr ASTStmt (Maybe ASTStmt) -- condition if else?
             | GoToStmt String
             | LabeledStmt String ASTStmt
             | CompoundStmt Block
             | BreakStmt (Maybe String)
             | ContinueStmt (Maybe String)
             | WhileStmt ASTExpr ASTStmt (Maybe String)
             | DoWhileStmt ASTStmt ASTExpr (Maybe String)
             | ForStmt ForInit (Maybe ASTExpr) (Maybe ASTExpr) ASTStmt (Maybe String)
             | NullStmt
             | SwitchStmt ASTExpr ASTStmt (Maybe String) (Maybe [CaseLabel])
             | CaseStmt ASTExpr ASTStmt (Maybe String)
             | DefaultStmt ASTStmt (Maybe String)

data ASTExpr = Factor Factor
             | ASTBinary BinOp ASTExpr ASTExpr
             | ASTAssign ASTExpr ASTExpr
             | ASTPostAssign ASTExpr PostOp
             | Conditional ASTExpr ASTExpr ASTExpr
             deriving (Eq)

data Factor = ASTLit Int
             | ASTUnary UnaryOp Factor
             | FactorExpr ASTExpr
             | ASTVar String
             | FunctionCall String [ASTExpr]
             deriving (Eq)

data CaseLabel = IntCase Int | DefaultCase
  deriving (Show, Eq)

data Declaration = VarDclr VariableDclr | FunDclr FunctionDclr
  deriving (Show)

data StorageClass = Static | Extern
  deriving (Show, Eq)

data TypeSpecifier = IntSpecifier -- eventually other types will be supported
  deriving (Show)

data FunPrefix = TypePrefix TypeSpecifier | StoragePrefix StorageClass

data FunctionDclr = FunctionDclr String (Maybe StorageClass) [VariableDclr] (Maybe Block)

data VariableDclr = VariableDclr String (Maybe StorageClass) (Maybe ASTExpr)
  deriving (Show)

data Type_ = IntType | UnsignedType | FunType Int -- param count
  deriving (Show, Eq)

data ForInit = InitDclr VariableDclr | InitExpr (Maybe ASTExpr)
  deriving (Show)

data UnaryOp = Complement
             | Negate
             | BoolNot
             deriving (Show, Eq)

data BinOp = SubOp | AddOp | MulOp | DivOp | ModOp |
             BitAnd | BitOr  | BitXor | BitShr | BitShl |
             BoolAnd | BoolOr | BoolEq | BoolNeq |
             BoolLe | BoolGe | BoolLeq | BoolGeq |
             AssignOp | PlusEqOp | MinusEqOp | TimesEqOp |
             DivEqOp | ModEqOp | AndEqOp | OrEqOp | XorEqOp |
             ShlEqOp | ShrEqOp | TernaryOp
            deriving (Show, Eq)

data PostOp = PostInc | PostDec
  deriving (Eq, Show)

compoundOps :: [BinOp]
compoundOps = [PlusEqOp, MinusEqOp, TimesEqOp, DivEqOp, ModEqOp, 
               AndEqOp, OrEqOp, XorEqOp, ShlEqOp, ShrEqOp]

relationalOps :: [BinOp]
relationalOps = [BoolEq, BoolNeq, BoolGe, BoolGeq, BoolLe, BoolLeq]

-- print the AST in a (hopefully) readable way
instance Show ASTProg where
  show (ASTProg ds) = "Program(\n" ++ unlines (showDeclaration 1 <$> ds) ++ "\n)"

showFunc :: Int -> FunctionDclr -> String
showFunc n (FunctionDclr name storageClass params (Just (Block body))) =
  tabs ++ "FunctionDef(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++
  tabs ++ "    params=" ++ show params ++ "\n"++
  tabs ++ "    body=[\n" ++
  unlines (showBlockItem (n + 2) <$> body) ++
  tabs ++ "    ]\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showFunc n (FunctionDclr name storageClass params Nothing) =
  tabs ++ "FunctionDclr(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++
  tabs ++ "    params=" ++ show params ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show FunctionDclr where
  show = showFunc 0

instance Show BlockItem where
  show = showBlockItem 1

showBlockItem :: Int -> BlockItem -> String
showBlockItem n (StmtBlock x) = showStatement n x ++ ","
showBlockItem n (DclrBlock x) = showDeclaration n x ++ ","

showDeclaration :: Int -> Declaration -> String
showDeclaration n (FunDclr f) =
  tabs ++ "Dclr(\n" ++ showFunc (n + 1) f ++
    "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showDeclaration n (VarDclr v) =
  tabs ++ "Dclr(" ++ show v ++ ")"
  where tabs = replicate (4 * n) ' '

showStatement :: Int -> ASTStmt -> String
showStatement n (RetStmt expr) =
  tabs ++ "Return(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (ExprStmt expr) =
  tabs ++ "ExprStmt(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (IfStmt expr stmt1 stmt2) =
  tabs ++ "IfStmt(" ++ show expr ++ ",\n" ++
    showStatement (n + 1) stmt1 ++
    maybe "" (\stmt -> ",\n" ++ showStatement (n + 1) stmt) stmt2 ++
    "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n NullStmt =
  tabs ++ "NullStmt"
  where tabs = replicate (4 * n) ' '
showStatement n (GoToStmt label) =
  tabs ++ "GoToStmt " ++ label
  where tabs = replicate (4 * n) ' '
showStatement n (LabeledStmt label stmt) =
  tabs ++ "LabeledStmt " ++ label ++ ":\n" ++ showStatement (n + 1) stmt
  where tabs = replicate (4 * n) ' '
showStatement n (CompoundStmt (Block items)) =
  tabs ++ "CompoundStmt:\n" ++ intercalate "\n" (showBlockItem (n + 1) <$> items)
  where tabs = replicate (4 * n) ' '
showStatement n (BreakStmt label) =
  tabs ++ "BreakStmt(" ++ show label ++")"
  where tabs = replicate (4 * n) ' '
showStatement n (ContinueStmt label) =
  tabs ++ "ContinueStmt(" ++ show label ++")"
  where tabs = replicate (4 * n) ' '
showStatement n (WhileStmt condition body label) =
  tabs ++ "WhileStmt(" ++ show condition ++ ",\n" ++
  showStatement (n + 1) body ++ "\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (DoWhileStmt body condition label) =
  tabs ++ "DoWhileStmt(\n" ++ showStatement (n + 1) body ++ ",\n" ++
  tabs ++ "    " ++ show condition ++ ",\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (ForStmt init condition end body label) =
  tabs ++ "ForStmt(" ++ show init ++ ",\n" ++
  tabs ++ "    " ++ showMaybe condition ++ ",\n" ++
  tabs ++ "    " ++ showMaybe end ++ ",\n" ++
  showStatement (n + 1) body ++ "\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (SwitchStmt expr stmt label (Just cases)) =
  tabs ++ "SwitchStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ "\n" ++ tabs ++
  "cases=\n" ++ 
  intercalate "\n" ((\s -> tabs ++ "    " ++ s) . show <$> cases) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (SwitchStmt expr stmt label Nothing) =
  tabs ++ "SwitchStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ "\n" ++ tabs ++
  "cases=Nothing)"
  where tabs = replicate (4 * n) ' '
showStatement n (CaseStmt expr stmt label) =
  tabs ++ "CaseStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (DefaultStmt stmt label) =
  tabs ++ "DefaultStmt(" ++ "\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '

showMaybe :: Show a => Maybe a -> String
showMaybe (Just x) = show x
showMaybe Nothing = ""

instance Show ASTStmt where
  show = showStatement 0

instance Show ASTExpr where
  show (Factor x) = show x
  show (ASTBinary op left right) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ ")"
  show (ASTAssign x y) = "Assign(" ++ show x ++ ", " ++ show y ++ ")"
  show (ASTPostAssign x op) = "PostAssign(" ++ show x ++ ", "++ show op ++ ")"
  show (Conditional c x y) =
    "Conditional(" ++ show c ++ ", " ++ show x ++ ", " ++ show y ++ ")"

instance Show Factor where
  show (ASTLit n) = "Constant(" ++ show n ++ ")"
  show (ASTUnary op expr) = show op ++ "(" ++ show expr ++ ")"
  show (FactorExpr expr) = show expr
  show (ASTVar x) = "Var(" ++ show x ++ ")"
  show (FunctionCall name args) =
    "FunctionCall(" ++ name ++ ", " ++ show args ++ ")"

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

isUnaryOp :: Token -> Bool
isUnaryOp op = case op of
  Minus -> True
  Tilde -> True
  Exclamation -> True
  _ -> False

isBinOp :: Token -> Bool
isBinOp op =
  case op of
    Minus -> True
    Plus -> True
    Asterisk -> True
    Slash -> True
    Percent -> True
    Ampersand -> True
    Pipe -> True
    Carat -> True
    ShiftLTok -> True
    ShiftRTok -> True
    DoubleAmpersand -> True
    DoublePipe -> True
    DoubleEquals -> True
    NotEqual -> True
    GreaterThan -> True
    LessThan -> True
    GreaterThanEq -> True
    LessThanEq -> True
    Equals -> True
    PlusEq -> True
    MinusEq -> True
    TimesEq -> True
    DivEq -> True
    ModEq -> True
    AndEq -> True
    OrEq -> True
    XorEq -> True
    ShlEq -> True
    ShrEq -> True
    Question -> True -- ternary operator can be parsed like a binary operator
    _ -> False

identName :: Token -> String
identName (Ident s) = s
identName x = error $ show x ++ " is not an identifier"

intLitVal :: Token -> Int
intLitVal (IntLit n) = n
intLitVal x = error $ show x ++ " is not an int lit"

getUnaryOp :: Token -> UnaryOp
getUnaryOp op = case op of
  Tilde -> Complement
  Minus -> Negate
  Exclamation -> BoolNot
  _ -> error $ show op ++ " is not an unary operator"

getBinOp :: Token -> BinOp
getBinOp op = case op of
  Plus -> AddOp
  Minus -> SubOp
  Asterisk -> MulOp
  Slash -> DivOp
  Percent -> ModOp
  Ampersand -> BitAnd
  Pipe -> BitOr
  Carat -> BitXor
  ShiftLTok -> BitShl
  ShiftRTok -> BitShr
  DoubleAmpersand -> BoolAnd
  DoublePipe -> BoolOr
  DoubleEquals -> BoolEq
  NotEqual -> BoolNeq
  GreaterThan -> BoolGe
  LessThan -> BoolLe
  GreaterThanEq -> BoolGeq
  LessThanEq -> BoolLeq
  Equals -> AssignOp
  PlusEq -> PlusEqOp
  MinusEq -> MinusEqOp
  TimesEq -> TimesEqOp
  DivEq -> DivEqOp
  ModEq -> ModEqOp
  AndEq -> AndEqOp
  OrEq -> OrEqOp
  XorEq -> XorEqOp
  ShlEq -> ShlEqOp
  ShrEq -> ShrEqOp
  Question -> TernaryOp
  _ -> error $ show op ++ " is not a binary operator"

getCompoundOp :: BinOp -> BinOp
getCompoundOp op = case op of
  PlusEqOp -> AddOp
  MinusEqOp -> SubOp
  TimesEqOp -> MulOp
  DivEqOp -> DivOp
  ModEqOp -> ModOp
  AndEqOp -> BitAnd
  OrEqOp -> BitOr
  XorEqOp -> BitXor
  ShlEqOp -> BitShl
  ShrEqOp -> BitShr
  _ -> error $ show op ++ " is not a compound binary operator"

getPrec :: BinOp -> Int
getPrec op = case op of
  MulOp -> 50
  DivOp -> 50
  ModOp -> 50
  AddOp -> 45
  SubOp -> 45
  BitShl -> 40
  BitShr -> 40
  BoolLe -> 35
  BoolGe -> 35
  BoolLeq -> 35
  BoolGeq -> 35
  BoolEq -> 30
  BoolNeq -> 30
  BitAnd -> 25
  BitXor -> 20
  BitOr -> 15
  BoolAnd -> 10
  BoolOr -> 5
  TernaryOp -> 3
  AssignOp -> 1
  PlusEqOp -> 1
  MinusEqOp -> 1
  TimesEqOp -> 1
  DivEqOp -> 1
  ModEqOp -> 1
  AndEqOp -> 1
  OrEqOp -> 1
  XorEqOp -> 1
  ShlEqOp -> 1
  ShrEqOp -> 1

parseProgram :: Parser Token ASTProg
parseProgram = ASTProg <$> many parseDclr <* parseEOF

parseFunction :: Parser Token FunctionDclr
parseFunction = do
  (type_, storageClass) <- parseTypeAndStorageClass
  name <- identName <$> satisfy isIdent
  params <- match OpenP *> ([] <$ match Void <* match CloseP <|> [] <$ match CloseP <|> some parseParam)
  FunctionDclr name storageClass params <$> parseEndOfFunc

parseEndOfFunc :: Parser Token (Maybe Block)
parseEndOfFunc = do
  body <- optional parseBlock
  case body of
    Just b -> return body -- function definition
    Nothing -> Nothing <$ match Semi -- function declaration

parseParam :: Parser Token VariableDclr
parseParam = liftA3 VariableDclr
  (match IntTok *> (identName <$> satisfy isIdent) <* (match Comma <|> match CloseP))
  (pure Nothing) -- params don't have storage specifiers
  (pure Nothing) -- params aren't initialized with an expression

parseBlock :: Parser Token Block
parseBlock = match OpenB *> (Block <$> some parseBlockItem) <* match CloseB

parseBlockItem :: Parser Token BlockItem
parseBlockItem =
  StmtBlock <$> parseStmt <|>
  DclrBlock <$> parseDclr

parseStmt :: Parser Token ASTStmt
parseStmt =
  RetStmt <$> (match ReturnTok *> parseExpr <* match Semi) <|>
  ExprStmt <$> (parseExpr <* match Semi) <|>
  liftA3 IfStmt
    (match IfTok *> match OpenP *> parseExpr <* match CloseP)
    parseStmt (optional (match ElseTok *> parseStmt)) <|>
  liftA2 LabeledStmt (identName <$> satisfy isIdent <* match Colon) parseStmt <|>
  GoToStmt . identName <$> (match GoToTok *> satisfy isIdent <* match Semi) <|>
  CompoundStmt <$> parseBlock <|>
  BreakStmt Nothing <$ (match BreakTok <* match Semi) <|>
  ContinueStmt Nothing <$ (match ContinueTok <* match Semi) <|>
  match WhileTok *> liftA3 WhileStmt (match OpenP *> parseExpr <* match CloseP)
    parseStmt (pure Nothing) <|>
  match DoTok *> liftA3 DoWhileStmt parseStmt
    (match WhileTok *> match OpenP *> parseExpr <* match CloseP <* match Semi) (pure Nothing) <|>
  match ForTok *> lift5 ForStmt
    (match OpenP *> parseForInit)
    (optional parseExpr <* match Semi)
    (optional parseExpr <* match CloseP)
    parseStmt (pure Nothing) <|>
  match SwitchTok *> lift4 SwitchStmt parseExpr parseStmt (pure Nothing) (pure Nothing) <|>
  match CaseTok *> liftA3 CaseStmt (parseExpr <* match Colon) parseStmt (pure Nothing) <|>
  match DefaultTok *> match Colon *> liftA2 DefaultStmt parseStmt (pure Nothing) <|>
  NullStmt <$ match Semi

parseForInit :: Parser Token ForInit
parseForInit =
  InitDclr <$> parseVariableDclr <|>
  InitExpr <$> optional parseExpr <* match Semi

parseDclr :: Parser Token Declaration
parseDclr = VarDclr <$> parseVariableDclr <|>
            FunDclr <$> parseFunction

parseVariableDclr :: Parser Token VariableDclr
parseVariableDclr = do
  (type_, storageClass) <- parseTypeAndStorageClass
  name <- identName <$> satisfy isIdent
  expr <- optional (match Equals *> parseExpr) <* match Semi
  return (VariableDclr name storageClass expr)

-- parses a type specifier or a storage class
parseTypeOrStorageClass :: Parser Token FunPrefix
parseTypeOrStorageClass = do
  prefix <- match IntTok <|> (match StaticTok <|> match ExternTok)
  case prefix of
    IntTok -> return (TypePrefix IntSpecifier)
    StaticTok -> return (StoragePrefix Static)
    ExternTok -> return (StoragePrefix Extern)
    x -> errorParse $ "Invalid function prefix: " ++ show x

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
parseTypeAndStorageClass :: Parser Token (TypeSpecifier, Maybe StorageClass)
parseTypeAndStorageClass = do
  (types, storageClasses) <- parseTypesAndStorageClasses
  if length types /= 1 then
    errorParse $ "Invalid type specifier: " ++ show types
  else if length storageClasses > 1 then
    errorParse $ "Invalid storage class: " ++ show storageClasses
  else
    return (IntSpecifier, safeHead storageClasses)

parseLit :: Parser Token Factor
parseLit = ASTLit . intLitVal <$> satisfy isIntLit

parseUnary :: Parser Token Factor
parseUnary = ASTUnary . getUnaryOp <$> satisfy isUnaryOp <*> parseFactor <|>
             parsePreIncDec

parsePreIncDec :: Parser Token Factor
parsePreIncDec = do
  op <- match IncTok <|> match DecTok
  v <- parseVar
  let binop = if op == IncTok then PlusEqOp else MinusEqOp
  return (FactorExpr $ ASTBinary binop (Factor v) (Factor $ ASTLit 1))

-- adds next factor to binary expression
-- needed to make operators left-associative
parseBinExpand :: Parser Token ASTExpr -> Int -> Parser Token (Maybe ASTExpr)
parseBinExpand parser minPrec =
  expanded <|> pure Nothing where
  expanded = do
    left <- parser
    op <- getBinOp <$> satisfy isBinOp
    let nextPrec = getPrec op
    if nextPrec >= minPrec
    then do
        -- higher precedence: create new ASTExpr
        if op == AssignOp then do
          right <- parseBin (Factor <$> parseFactor) nextPrec
          return . Just $ ASTAssign left right
        else if op == TernaryOp then do
          middle <- parseExpr <* match Colon
          right <- parseBin (Factor <$> parseFactor) nextPrec
          return . Just $ Conditional left middle right
        else do
          right <- parseBin (Factor <$> parseFactor) (nextPrec + 1)
          return . Just $ ASTBinary op left right
    else empty -- will just use left expr

-- need monadic parsing to avoid left-recursion
parseBin :: Parser Token ASTExpr -> Int -> Parser Token ASTExpr
parseBin parser minPrec = do
  mEnd <- parseBinExpand parser minPrec
  case mEnd of
    Nothing -> parser
    Just newLeft -> parseBin (pure newLeft) minPrec

parseParens :: Parser Token Factor
parseParens = match OpenP *> (FactorExpr <$> parseExpr) <* match CloseP

parseFactor :: Parser Token Factor
parseFactor = parseLit <|>
              parseUnary <|>
              parseParens <|>
              parsePostIncDec <|>
              liftA2 FunctionCall (identName <$> satisfy isIdent)
                (match OpenP *> ([] <$ match CloseP <|> some parseArg)) <|>
              parseVar

parseArg :: Parser Token ASTExpr
parseArg = parseExpr <*
  (match Comma <|> match CloseP)

parseVar :: Parser Token Factor
parseVar = ASTVar . identName <$> satisfy isIdent

parsePostIncDec :: Parser Token Factor
parsePostIncDec = do
  v <- parseVar
  op <- match IncTok <|> match DecTok
  let postOp = if op == IncTok then PostInc else PostDec
  return (FactorExpr $ ASTPostAssign (Factor v) postOp)

parseExpr :: Parser Token ASTExpr
parseExpr = parseBin (Factor <$> parseFactor) 0

-- to ensure the entire file was parsed
parseEOF :: Parser Token ()
parseEOF = Parser f
  where
    f ts
      | null ts = Right ((), ts)
      | otherwise = Left $ "Syntax Error: Could not parse " ++ show ts

safeHead :: [a] -> Maybe a
safeHead (x : _) = Just x
safeHead [] = Nothing