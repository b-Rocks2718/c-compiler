
module Parser where

import Lexer
import Control.Applicative

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

testAST :: Program
testAST = Program $ Function "main" $ Statement $ Expr 2

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

identName :: Token -> String
identName (Ident s) = s
identName _ = "THIS IS NOT AN IDENTIFIER!"

parseProgram :: Parser Token Program
parseProgram = Program <$> parseFunction

parseFunction :: Parser Token Function
parseFunction = liftA2 Function (identName <$> satisfy isIdent) (parseStatement)

parseStatement :: Parser Token Statement
parseStatement = Parser $ const (Right (Statement $ Expr 0, []))

parseExpr :: Parser Token Expr
parseExpr = undefined
