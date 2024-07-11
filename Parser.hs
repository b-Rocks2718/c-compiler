
module Parser where

import Lexer
import Control.Applicative

-- AST data structure
data ASTProg = ASTProg ASTFunc
data ASTFunc = ASTFunc String ASTStmt
data ASTStmt = ASTReturn ASTExpr
data ASTExpr = ASTLit Int
             | ASTUnary UnaryOp ASTExpr

data UnaryOp = Complement
             | Negate
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

-- only return statements for now
showStatement :: ASTStmt -> Int -> String
showStatement (ASTReturn expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show ASTStmt where
  show = flip showStatement 0

instance Show ASTExpr where
  show (ASTLit n) = "Constant(" ++ show n ++ ")"
  show (ASTUnary Complement expr) =
    "Complement(" ++ show expr ++ ")"
  show (ASTUnary Negate expr) =
    "Negate(" ++ show expr ++ ")"

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

isUnaryOp :: Token -> Bool
isUnaryOp Minus = True
isUnaryOp Tilde = True
isUnaryOp _ = False

identName :: Token -> String
identName (Ident s) = s
identName _ = error "not an identifier"

intLitVal :: Token -> Int
intLitVal (IntLit n) = n
intLitVal _ = error "not an int lit"

getUnaryOp :: Token -> UnaryOp
getUnaryOp Tilde = Complement
getUnaryOp Minus = Negate
getUnaryOp _ = error "not an unary operator"

-- Program is just a function
parseProgram :: Parser Token ASTProg
parseProgram = ASTProg <$> parseFunction <* parseEOF

-- function looks like ident(void){stmt}
parseFunction :: Parser Token ASTFunc
parseFunction = liftA2 ASTFunc
  (identName <$> (match IntTok *> (satisfy isIdent) <* match OpenP <*
                 tryParse Void <* match CloseP <* match OpenB))
  (parseStatement) <* (match CloseB)

-- stmt is just return exp;
parseStatement :: Parser Token ASTStmt
parseStatement =
  ASTReturn <$> ((match ReturnTok) *> parseExpr <* (match Semi))

parseLit :: Parser Token ASTExpr
parseLit = ASTLit <$> intLitVal <$> (satisfy isIntLit)

parseUnary :: Parser Token ASTExpr
parseUnary = ASTUnary <$> getUnaryOp <$> (satisfy isUnaryOp) <*>
             parseExpr

parseParens :: Parser Token ASTExpr
parseParens = match OpenP *> parseExpr <* match CloseP

-- exp is just an integer literal or unary op on an exp
parseExpr :: Parser Token ASTExpr
parseExpr = parseLit <|>
            parseUnary <|>
            parseParens

-- to ensure the entire file was parsed
parseEOF :: Parser Token [a]
parseEOF = Parser f
  where
    f ts
      | null ts = Right ([], ts)
      | otherwise = Left $ "Could not lex: " ++ show ts

tryParse :: Eq a => a -> Parser a [a]
tryParse a = (:[]) <$> (match a) <|> pure []

showAST :: Either String (ASTProg, [Token]) -> String
showAST (Right (p, ts)) = show p
showAST (Left s) = s

showTokens :: Either String ([Token], String) -> String
showTokens (Right (ts, s)) = show ts
showTokens (Left s) = s
