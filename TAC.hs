
module TAC where

import Lexer
import Parser

data TACProg  = TACProg TACFunc deriving (Show)
data TACFunc  = TACFunc String [TACInstr]
data TACInstr = TACReturn TACVal
              | TACUnary UnaryOp TACVal TACVal -- op dst src
              | TACBinary BinOp TACVal TACVal TACVal -- op dst scr1 scr2
              | TACCondJump Condition String
              | TACCmp TACVal TACVal
              | TACJump String
              | TACLabel String
              | TACCopy TACVal TACVal
              deriving (Show)

-- temporary variable type
data TACVal   = TACLit Int
              | TACVar String
              deriving (Show)

data Condition = CondE | CondNE | CondG | CondGE | CondL | CondLE 
  deriving(Show)

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

-- create a temporary variable name
makeTemp :: String -> TACVal -> String
makeTemp name rslt = 
  case rslt of 
    (TACVar srcName) -> 
      init srcName ++ show (read [last srcName] + 1)
    (TACLit n) -> 
      name

-- name has a counter appended to ensure tmp variables are unique
-- function returns instructions and return value
factorToTAC :: String -> Factor -> ([TACInstr], TACVal)
factorToTAC name factor = 
  case factor of 
    (ASTLit n) -> ([], TACLit n)
    (ASTUnary op expr) -> 
      (fst rslt ++ [TACUnary op dst src], dst)
      where rslt = factorToTAC name expr
            src = snd rslt
            dstName = makeTemp name src
            dst = TACVar dstName
    (FactorExpr e) -> exprToTAC name e

relationToCond :: BinOp -> Condition
relationToCond op = case op of
  BoolEq -> CondE
  BoolNeq -> CondNE
  BoolGe -> CondG
  BoolGeq -> CondGE
  BoolLe -> CondL
  BoolLeq -> CondLE

relationalToTAC :: String -> BinOp -> ASTExpr -> ASTExpr -> ([TACInstr], TACVal)
relationalToTAC name op left right = 
  ([TACCopy dst (TACLit 1)] ++ 
  fst rslt1 ++ fst rslt2 ++
  [TACCmp src1 src2, 
  TACCondJump (relationToCond op) endStr,
  TACCopy dst (TACLit 0),
  TACLabel endStr], dst)
  where rslt1 = exprToTAC name left
        src1 = snd rslt1
        dstName1 = makeTemp name src1
        rslt2 = exprToTAC dstName1 right
        src2 = snd rslt2
        dstName2 = makeTemp dstName1 src2
        dst = TACVar dstName2
        n = show (read [last dstName2] :: Int)
        endStr = "end." ++ n

exprToTAC :: String -> ASTExpr -> ([TACInstr], TACVal)
exprToTAC name expr =
  case expr of
    (Factor f) -> factorToTAC name f
    -- short-circuiting operators
    (ASTBinary BoolAnd left right) -> 
      ([TACCopy dst (TACLit 0)] ++ 
      fst rslt1 ++ 
      [TACCmp src1 (TACLit 0),
      TACCondJump CondE endStr] ++ 
      fst rslt2 ++
      [TACCmp src2 (TACLit 0), 
      TACCondJump CondE endStr, 
      TACCopy dst (TACLit 1),
      TACLabel endStr], dst)
      where rslt1 = exprToTAC name left
            src1 = snd rslt1
            dstName1 = makeTemp name src1
            rslt2 = exprToTAC dstName1 right
            src2 = snd rslt2
            dstName2 = makeTemp dstName1 src2
            dst = TACVar dstName2
            n = show (read [last dstName2] :: Int)
            endStr = "end." ++ n
    (ASTBinary BoolOr left right) ->
      ([TACCopy dst (TACLit 1)] ++ 
      fst rslt1 ++ 
      [TACCmp src1 (TACLit 0), 
      TACCondJump CondNE endStr] ++ 
      fst rslt2 ++
      [TACCmp src2 (TACLit 0), 
      TACCondJump CondNE endStr, 
      TACCopy dst (TACLit 0),
      TACLabel endStr], dst)
      where rslt1 = exprToTAC name left
            src1 = snd rslt1
            dstName1 = makeTemp name src1
            rslt2 = exprToTAC dstName1 right
            src2 = snd rslt2
            dstName2 = makeTemp dstName1 src2
            dst = TACVar dstName2
            n = show (read [last dstName2] :: Int)
            endStr = "end." ++ n
    -- relational operators (probably should rewrite)
    (ASTBinary BoolEq left right) -> relationalToTAC name BoolEq left right
    (ASTBinary BoolNeq left right) -> relationalToTAC name BoolNeq left right
    (ASTBinary BoolGe left right) -> relationalToTAC name BoolGe left right
    (ASTBinary BoolGeq left right) -> relationalToTAC name BoolGeq left right
    (ASTBinary BoolLe left right) -> relationalToTAC name BoolLe left right
    (ASTBinary BoolLeq left right) -> relationalToTAC name BoolLeq left right
    -- arithmetic operators (single instruction)
    -- could move mul/div/mod from a function call to here
    (ASTBinary op left right) ->
      (fst rslt1 ++ fst rslt2 ++ [TACBinary op dst src1 src2], dst)
      where rslt1 = exprToTAC name left
            src1 = snd rslt1
            dstName1 = makeTemp name src1
            rslt2 = exprToTAC dstName1 right
            src2 = snd rslt2
            dstName2 = makeTemp dstName1 src2
            dst = TACVar dstName2 -- possible optimization: use dstName1