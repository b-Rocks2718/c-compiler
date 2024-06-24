
module Parser where

import Lexer
import Control.Applicative
import System.Environment

data Program   = Program Function
data Function  = Function String Statement
data Statement = Statement Expr -- just return statements for now
data Expr      = Expr Int -- just constant int literals for now

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
showStatement (Statement expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show Statement where
  show = flip showStatement 0

instance Show Expr where
  show (Expr n) = "Constant(" ++ show n ++ ")"

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

identName :: Token -> String
identName (Ident s) = s
identName _ = "THIS IS NOT AN IDENTIFIER!"

intLitVal :: Token -> Int
intLitVal (IntLit n) = n
intLitVal _ = undefined -- hopefully we never end up here

-- (for now) Program is just a function
parseProgram :: Parser Token Program
parseProgram = Program <$> parseFunction <* parseEOF

-- (for now) function looks like ident(void){stmt}
parseFunction :: Parser Token Function
parseFunction = liftA2 Function
  (identName <$> (match Int_ *> (satisfy isIdent) <* match OpenP <*
                 tryParse Void <* match CloseP <* match OpenB))
  (parseStatement) <* (match CloseB)

-- (for now) stmt looks like return exp;
parseStatement :: Parser Token Statement
parseStatement =
  Statement <$> ((match Return) *> parseExpr <* (match Semi))

-- (for now) exp is just an integer literal
parseExpr :: Parser Token Expr
parseExpr = Expr <$> intLitVal <$> (satisfy isIntLit)

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

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  content <- readFile path
  putStrLn ("Input program:\n" ++ content)
  let result = preprocess content
  putStrLn ("Preprocessed code:\n" ++ result)
  let tokens = lexerEval result
  putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = (fst <$> tokens) >>= (runParser parseProgram)
  putStrLn ("\nSyntax tree:\n" ++ showAST ast)
