
module TAC where

import Lexer
import Parser

data TACProg  = TACProg TACFunc deriving (Show)
data TACFunc  = TACFunc String [TACInstr]
data TACInstr = TACReturn TACVal
              | TACUnary UnaryOp TACVal TACVal -- op dst src
              deriving (Show)

instance Show TACFunc where
  show (TACFunc name instrs) =
    "TACFunc " ++ show name ++ ":\n" ++
    (unlines $ show <$> instrs)

-- temporary variable type
data TACVal   = TACLit Int
              | TACVar String
              deriving (Show)

progToTAC :: ASTProg -> TACProg
progToTAC (ASTProg f) = TACProg $ funcToTAC f

funcToTAC :: ASTFunc -> TACFunc
funcToTAC (ASTFunc name body) = TACFunc name (stmtToTAC name body)

stmtToTAC :: String -> ASTStmt -> [TACInstr]
stmtToTAC name (ASTReturn expr) = (fst rslt) ++ [TACReturn $ snd rslt]
  where rslt = exprToTAC name expr

exprToTAC :: String -> ASTExpr -> ([TACInstr], TACVal)
exprToTAC name (ASTLit n) = ([], TACLit n)
exprToTAC name (ASTUnary op (ASTLit n)) =
  ([TACUnary op dst (TACLit n)], dst)
  where dst = TACVar $ name ++ ".tmp.0"
exprToTAC name (ASTUnary op expr) =
  ((fst rslt) ++ [TACUnary op dst src], dst)
  where rslt = exprToTAC name expr
        src@(TACVar srcName) = snd rslt -- dst is always a variable
        n = show (read [last srcName] + 1) -- increment counter
        dst = TACVar $ name ++ ".tmp." ++ n
