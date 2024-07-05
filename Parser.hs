
module Parser where

import Lexer
import Control.Applicative

data Program   = Program Function
data Function  = Function String Statement
data Statement = Return Expr
data Expr      = Lit Int
               | Unary UnaryOp Expr

data UnaryOp = Complement
             | Negate

instance Show Program where
  show (Program f) = "Program(\n" ++ showFunc f 1 ++ "\n)"

showFunc :: Function -> Int -> String
showFunc (Function name body) n =
  tabs ++ "Function(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++ tabs ++ "    body=" ++ showStatement body (n + 1) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show Function where
  show = flip showFunc 0

-- only return statements for now
showStatement :: Statement -> Int -> String
showStatement (Return expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show Statement where
  show = flip showStatement 0

instance Show Expr where
  show (Lit n) = "Constant(" ++ show n ++ ")"
  show (Unary Complement expr) =
    "Complement(" ++ show expr ++ ")"
  show (Unary Negate expr) =
    "Negate(" ++ show expr ++ ")"

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

isUnaryOp :: Token -> Bool
isUnaryOp Neg = True
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
getUnaryOp Neg = Negate
getUnaryOp _ = error "not an unary operator"

-- Program is just a function
parseProgram :: Parser Token Program
parseProgram = Program <$> parseFunction <* parseEOF

-- function looks like ident(void){stmt}
parseFunction :: Parser Token Function
parseFunction = liftA2 Function
  (identName <$> (match Int_ *> (satisfy isIdent) <* match OpenP <*
                 tryParse Void <* match CloseP <* match OpenB))
  (parseStatement) <* (match CloseB)

-- stmt is just return exp;
parseStatement :: Parser Token Statement
parseStatement =
  Return <$> ((match Return_) *> parseExpr <* (match Semi))

parseLit :: Parser Token Expr
parseLit = Lit <$> intLitVal <$> (satisfy isIntLit)

parseUnary :: Parser Token Expr
parseUnary = Unary <$> getUnaryOp <$> (satisfy isUnaryOp) <*>
             parseExpr

parseParens :: Parser Token Expr
parseParens = match OpenP *> parseExpr <* match CloseP

-- exp is just an integer literal or unary op on an exp
parseExpr :: Parser Token Expr
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

showAST :: Either String (Program, [Token]) -> String
showAST (Right (p, ts)) = show p
showAST (Left s) = s

showTokens :: Either String ([Token], String) -> String
showTokens (Right (ts, s)) = show ts
showTokens (Left s) = s
