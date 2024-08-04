module Parser where

import Lexer
import Control.Applicative

-- AST data structure
data ASTProg = ASTProg ASTFunc
data ASTFunc = ASTFunc String [BlockItem]
data BlockItem = StmtBlock ASTStmt | DclrBlock ASTDclr 
data ASTStmt = ASTReturn ASTExpr
             | ExprStmt ASTExpr
             | NullStmt
data ASTExpr = Factor Factor
             | ASTBinary BinOp ASTExpr ASTExpr
             | ASTAssign ASTExpr ASTExpr
data Factor = ASTLit Int
             | ASTUnary UnaryOp Factor
             | FactorExpr ASTExpr
             | ASTVar String
data ASTDclr = ASTDclr String (Maybe ASTExpr)
  deriving (Show)

data UnaryOp = Complement
             | Negate
             | BoolNot
             deriving (Show)

data BinOp = SubOp | AddOp | MulOp | DivOp | ModOp |
             BitAnd | BitOr  | BitXor | BitShr | BitShl |
             BoolAnd | BoolOr | BoolEq | BoolNeq | 
             BoolLe | BoolGe | BoolLeq | BoolGeq |
             AssignOp
            deriving (Show, Eq)

instance Show ASTProg where
  show (ASTProg f) = "Program(\n" ++ showFunc 1 f ++ "\n)"

showFunc :: Int -> ASTFunc -> String
showFunc n (ASTFunc name body) =
  tabs ++ "Function(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++ tabs ++ "    body=[\n" ++ 
  unlines (showBlockItem (n + 2) <$> body) ++ 
  tabs ++ "    ]\n" ++ tabs ++ ")" 
  where tabs = replicate (4 * n) ' '

instance Show ASTFunc where
  show = showFunc 0

instance Show BlockItem where
  show = showBlockItem 1

showBlockItem :: Int -> BlockItem -> String
showBlockItem n (StmtBlock x) = showStatement n x ++ ","
showBlockItem n (DclrBlock x) = showDeclaration n x ++ ","

showDeclaration :: Int -> ASTDclr -> String
showDeclaration n expr =
  tabs ++ "Dclr(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' ' 

showStatement :: Int -> ASTStmt -> String
showStatement n (ASTReturn expr) =
  tabs ++ "Return(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (ExprStmt expr) =
  tabs ++ "ExprStmt(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n NullStmt =
  tabs ++ "NullStmt"
  where tabs = replicate (4 * n) ' '

instance Show ASTStmt where
  show = showStatement 0

instance Show ASTExpr where
  show (Factor x) = show x
  show (ASTBinary op left right) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ ")"
  show (ASTAssign x y) = "Assign(" ++ show x ++ ", " ++ show y ++ ")"

instance Show Factor where
  show (ASTLit n) = "Constant(" ++ show n ++ ")"
  show (ASTUnary op expr) = show op ++ "(" ++ show expr ++ ")"
  show (FactorExpr expr) = show expr
  show (ASTVar x) = "Var(" ++ show x ++ ")"

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
  _ -> error $ show op ++ " is not a binary operator"

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
  BitOr  -> 15
  BoolAnd -> 10
  BoolOr -> 5
  AssignOp -> 1

parseProgram :: Parser Token ASTProg
parseProgram = ASTProg <$> parseFunction <* parseEOF

parseFunction :: Parser Token ASTFunc
parseFunction = liftA2 ASTFunc
  (identName <$> (match IntTok *> satisfy isIdent <* match OpenP <*
                 optional (match Void) <* match CloseP <* match OpenB))
  (some parseBlockItem) <* match CloseB

parseStmt :: Parser Token ASTStmt
parseStmt = 
  ASTReturn <$> (match ReturnTok *> parseExpr <* match Semi) <|>
  ExprStmt <$> (parseExpr <* match Semi) <|>
  NullStmt <$ match Semi

parseDclr :: Parser Token ASTDclr
parseDclr = liftA2 ASTDclr 
  (identName <$> (match IntTok *> satisfy isIdent)) 
  (maybeParse (match Equals *> parseExpr) <* match Semi)

parseBlockItem :: Parser Token BlockItem
parseBlockItem = 
  StmtBlock <$> parseStmt <|>
  DclrBlock <$> parseDclr

parseLit :: Parser Token Factor
parseLit = ASTLit . intLitVal <$> satisfy isIntLit

parseUnary :: Parser Token Factor
parseUnary = ASTUnary . getUnaryOp <$> satisfy isUnaryOp <*>
             parseFactor

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
              parseVar

parseVar :: Parser Token Factor
parseVar = ASTVar . identName <$> satisfy isIdent

parseExpr :: Parser Token ASTExpr
parseExpr = parseBin (Factor <$> parseFactor) 0

-- to ensure the entire file was parsed
parseEOF :: Parser Token ()
parseEOF = Parser f
  where
    f ts
      | null ts = Right ((), ts)
      | otherwise = Left $ "Could not lex: " ++ show ts