
module TAC where

import Lexer
import Parser

{-   TAC Intermediate Representation:
program = Program(function_definition)
function_definition = Function(identifier, instruction* body)
instruction = Return(val)
 | Unary(unary_operator, val dst, val src)
 | Binary(binary_operator, val dst, val src1, val src2)
val = Constant(int) | Var(identifier)
unary_operator = Complement | Negate
binary_operator = Add | Subtract | Multiply | Divide | Remainder
-}

data TACProg  = TACProg TACFunc deriving (Show)
data TACFunc  = TACFunc String [TACInstr]
data TACInstr = TACReturn TACVal
              | TACUnary UnaryOp TACVal TACVal -- op dst src
              | TACBinary BinOp TACVal TACVal TACVal -- op dst scr1 scr2
              deriving (Show)

-- temporary variable type
data TACVal   = TACLit Int
              | TACVar String
              deriving (Show)

instance Show TACFunc where
  show (TACFunc name instrs) =
    "TACFunc " ++ show name ++ ":\n" ++
    unlines (show <$> instrs)

progToTAC :: ASTProg -> TACProg
progToTAC (ASTProg f) = TACProg $ funcToTAC f

funcToTAC :: ASTFunc -> TACFunc
funcToTAC (ASTFunc name body) = TACFunc name (stmtToTAC name body)

stmtToTAC :: String -> ASTStmt -> [TACInstr]
stmtToTAC name (ASTReturn expr) = fst rslt ++ [TACReturn (snd rslt)]
  where rslt = exprToTAC (name ++ ".tmp.0") expr

makeTemp :: String -> ([TACInstr], TACVal) -> (TACVal, String)
makeTemp name rslt = (src, init name ++ n)
  where (src, srcName, n) = 
          case snd rslt of 
            src@(TACVar srcName) -> 
              (src, srcName, show (read [last srcName] + 1))
            src@(TACLit n) -> 
              (src, name, show (read [last srcName] + 0))

-- name has a counter appended to ensure tmp variables are unique
-- function returns instructions and return value
factorToTAC :: String -> Factor -> ([TACInstr], TACVal)
factorToTAC name factor = 
  case factor of 
    (ASTLit n) -> ([], TACLit n)
    (ASTUnary op expr) -> 
      (fst rslt ++ [TACUnary op dst src], dst)
      where rslt = factorToTAC name expr
            (src, dstName) = makeTemp name rslt
            dst = TACVar dstName
    (FactorExpr e) -> exprToTAC name e

exprToTAC :: String -> ASTExpr -> ([TACInstr], TACVal)
exprToTAC name expr =
  case expr of
    (Factor f) -> factorToTAC name f
    (ASTBinary op left right) -> 
      (fst rslt1 ++ fst rslt2 ++ [TACBinary op dst src1 src2], dst)
      where rslt1 = exprToTAC name left
            (src1, dstName1) = makeTemp name rslt1
            rslt2 = exprToTAC dstName1 right
            (src2, dstName2) = makeTemp dstName1 rslt2
            dst = TACVar dstName2 -- possible optimization: use dstName1

