
module Parser where

import Lexer
import Control.Applicative

-- AST data structure
data ASTProg = ASTProg ASTFunc
data ASTFunc = ASTFunc String ASTStmt
data ASTStmt = ASTReturn ASTExpr
data ASTExpr = Factor Factor
             | ASTBinary BinOp ASTExpr ASTExpr
data Factor = ASTLit Int
             | ASTUnary UnaryOp Factor
             | FactorExpr ASTExpr

data UnaryOp = Complement
             | Negate
             | BoolNot
             deriving (Show)

data BinOp = SubOp | AddOp | MulOp | DivOp | ModOp |
             BitAnd | BitOr  | BitXor | BitShr | BitShl |
             BoolAnd | BoolOr | BoolEq | BoolNeq | 
             BoolLe | BoolGe | BoolLeq | BoolGeq
            deriving (Show)

instance Show ASTProg where
  show (ASTProg f) = "Program(\n" ++ showFunc f 1 ++ "\n)"

showFunc :: ASTFunc -> Int -> String
showFunc (ASTFunc name body) n =
  tabs ++ "Function(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++ tabs ++ "    body=" ++ showStatement body (n + 1) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show ASTFunc where
  show = flip showFunc 0

showStatement :: ASTStmt -> Int -> String
showStatement (ASTReturn expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show ASTStmt where
  show = flip showStatement 0

instance Show ASTExpr where
  show (Factor x) = show x
  show (ASTBinary op left right) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ ")"

instance Show Factor where
  show (ASTLit n) = "Constant(" ++ show n ++ ")"
  show (ASTUnary op expr) = show op ++ "(" ++ show expr ++ ")"
  show (FactorExpr expr) = show expr

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

parseProgram :: Parser Token ASTProg
parseProgram = ASTProg <$> parseFunction <* parseEOF

parseFunction :: Parser Token ASTFunc
parseFunction = liftA2 ASTFunc
  (identName <$> (match IntTok *> satisfy isIdent <* match OpenP <*
                 optional (match Void) <* match CloseP <* match OpenB))
  parseStatement <* match CloseB

parseStatement :: Parser Token ASTStmt
parseStatement =
  ASTReturn <$> (match ReturnTok *> parseExpr <* match Semi)

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
              parseParens

parseExpr :: Parser Token ASTExpr
parseExpr = parseBin (Factor <$> parseFactor) 0

-- to ensure the entire file was parsed
parseEOF :: Parser Token ()
parseEOF = Parser f
  where
    f ts
      | null ts = Right ((), ts)
      | otherwise = Left $ "Could not lex: " ++ show ts

-- print Eithers without 'Left'/'Right'
showAST :: Either String (ASTProg, [Token]) -> String
showAST (Right (p, ts)) = show p
showAST (Left s) = s

showTokens :: Either String ([Token], String) -> String
showTokens (Right (ts, s)) = show ts
showTokens (Left s) = s