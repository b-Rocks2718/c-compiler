
module Parser where

import Lexer

data Program   = Program Function
data Function  = Function String Statement -- name, body
data Statement = Statement Expr -- only return statements for now
data Expr      = Expr Int -- just constant int literals for now

instance Show Program where
  show (Program f) = "Program(\n" ++ showFunc f 1 ++ "\n)"

showFunc :: Function -> Int -> String
showFunc (Function name body) n =
  tabs ++ "Function(\n" ++ tabs ++ "    name=\"" ++ name ++
  "\",\n" ++ tabs ++ "    body=" ++ showStatement body (n + 1) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

-- only return statements for now
showStatement :: Statement -> Int -> String
showStatement (Statement expr) n =
  "Return(\n" ++ tabs ++ "    " ++ show expr ++ "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show Expr where
  show (Expr n) = "Constant(" ++ show n ++ ")"

testAST :: Program
testAST = Program $ Function "main" $ Statement $ Expr 2

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

parseProgram :: Parser Token Program
parseProgram = undefined

parseFunction :: Parser Token Function
parseFunction = undefined

parseStatement :: Parser Token Statement
parseStatement = undefined

parseExpr :: Parser Token Expr
parseExpr = undefined
