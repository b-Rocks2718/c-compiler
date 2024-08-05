module TAC where

import Lexer
import Parser
import Semantics
import Data.List ( foldl' )

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

type TACState = (TACVal, Int)

instance Show TACFunc where
  show (TACFunc name instrs) =
    "TACFunc " ++ show name ++ ":\n" ++
    unlines (show <$> instrs)

progToTAC :: ASTProg -> TACProg
progToTAC (ASTProg f) = TACProg $ funcToTAC f

-- TODO: consider using State Monad
-- initialize global counter here
funcToTAC :: ASTFunc -> TACFunc
funcToTAC (ASTFunc name body) = TACFunc name $ 
  fst (foldl' (blockItemsToTAC name) ([], (TACLit 0, 0)) body) ++ [TACReturn (TACLit 0)]

addInstrs :: [TACInstr] -> ([TACInstr], TACState) -> ([TACInstr], TACState)
addInstrs instrs (instrs', state) = (instrs ++ instrs', state)

blockItemsToTAC :: String -> ([TACInstr], TACState) -> BlockItem -> ([TACInstr], TACState)
blockItemsToTAC name (instrs, state) item = case item of
  StmtBlock stmt -> addInstrs instrs (stmtToTAC name state stmt)
  DclrBlock (ASTDclr varName mExpr) -> case mExpr of
    Just expr -> addInstrs instrs $ exprToTAC name state 
      (ASTAssign (Factor $ ASTVar varName) expr)
    Nothing -> (instrs, state)

stmtToTAC :: String -> TACState -> ASTStmt -> ([TACInstr], TACState)
stmtToTAC name state@(val, n) stmt = case stmt of 
  (ASTReturn expr) -> (instrs ++ [TACReturn rslt], (rslt, n'))
    where (instrs, (rslt, n')) = exprToTAC name state expr
  (ExprStmt expr) -> exprToTAC name state expr
  (IfStmt condition left right) -> case right of
    Just stmt -> ifElseToTAC name state condition left stmt
    Nothing -> ifToTAC name state condition left
  NullStmt -> ([], (val, n))

ifToTAC :: String -> TACState -> ASTExpr -> 
    ASTStmt -> ([TACInstr], TACState)
ifToTAC name state condition left = 
  (rslt1 ++ 
  [TACCmp src1 (TACLit 0),
   TACCondJump CondE endStr] ++
   rslt2 ++ 
  [TACLabel endStr], (dst, n4 + 1))
  where (rslt1, (src1, n1)) = exprToTAC name state condition
        (dstName1, n2) = makeTemp name n1
        (rslt2, (src2, n3)) = stmtToTAC name (src1, n2) left
        (dstName2, n4) = makeTemp name n3
        dst = TACVar dstName2
        endStr = "end." ++ show n4

ifElseToTAC :: String -> TACState -> ASTExpr -> 
    ASTStmt -> ASTStmt -> ([TACInstr], TACState)
ifElseToTAC name state condition left right = 
  (rslt1 ++ 
  [TACCmp src1 (TACLit 0),
   TACCondJump CondE elseStr] ++
   rslt2 ++
   [TACJump endStr,
   TACLabel elseStr] ++
   rslt3 ++
  [TACLabel endStr], (dst, n6 + 1))
  where (rslt1, (src1, n1)) = exprToTAC name state condition
        (dstName1, n2) = makeTemp name n1
        (rslt2, (src2, n3)) = stmtToTAC name (src1, n2) left
        (dstName2, n4) = makeTemp name n3
        elseStr = "else." ++ show n4
        (rslt3, (src3, n5)) = stmtToTAC name (src1, n4 + 1) right
        (dstName3, n6) = makeTemp name n5
        dst = TACVar dstName3
        endStr = "end." ++ show n6

-- create a temporary variable name
makeTemp :: String -> Int -> (String, Int)
makeTemp name n = (name ++ ".tmp." ++ show n, n + 1)

factorToTAC :: String -> TACState -> Factor -> ([TACInstr], TACState)
factorToTAC name state@(val, n) factor = 
  case factor of 
    (ASTLit m) -> ([], (TACLit m, n))
    (ASTUnary op expr) -> 
      (fst rslt ++ [TACUnary op dst src], (dst, n2))
      where rslt = factorToTAC name state expr
            (src, n1) = snd rslt
            (dstName, n2) = makeTemp name n1
            dst = TACVar dstName
    (FactorExpr e) -> exprToTAC name state e
    (ASTVar v) -> ([], (TACVar v, n))

relationToCond :: BinOp -> Condition
relationToCond op = case op of
  BoolEq -> CondE
  BoolNeq -> CondNE
  BoolGe -> CondG
  BoolGeq -> CondGE
  BoolLe -> CondL
  BoolLeq -> CondLE

relationalToTAC :: String -> TACState -> BinOp -> ASTExpr -> ASTExpr -> ([TACInstr], TACState)
relationalToTAC name state op left right = 
  ([TACCopy dst (TACLit 1)] ++ 
  rslt1 ++ rslt2 ++
  [TACCmp src1 src2, 
  TACCondJump (relationToCond op) endStr,
  TACCopy dst (TACLit 0),
  TACLabel endStr], (dst, n4 + 2))
  where (rslt1, (src1, n1)) = exprToTAC name state left
        (dstName1, n2) = makeTemp name n1
        (rslt2, (src2, n3)) = exprToTAC name (src1, n2) right
        (dstName2, n4) = makeTemp name n3
        dst = TACVar dstName2
        endStr = "end." ++ show n4

exprToTAC :: String -> TACState -> ASTExpr -> ([TACInstr], TACState)
exprToTAC name state expr =
  case expr of
    (Factor f) -> factorToTAC name state f
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
      TACLabel endStr], (dst, n4 + 1))
      where rslt1 = exprToTAC name state left
            (src1, n1) = snd rslt1
            (dstName1, n2) = makeTemp name n1
            rslt2 = exprToTAC name (src1, n2) right
            (src2, n3) = snd rslt2
            (dstName2, n4) = makeTemp name n3
            dst = TACVar dstName2
            endStr = "end." ++ show n4
    (ASTBinary BoolOr left right) ->
      ([TACCopy dst (TACLit 1)] ++ 
      fst rslt1 ++ 
      [TACCmp src1 (TACLit 0), 
      TACCondJump CondNE endStr] ++ 
      fst rslt2 ++
      [TACCmp src2 (TACLit 0), 
      TACCondJump CondNE endStr, 
      TACCopy dst (TACLit 0),
      TACLabel endStr], (dst, n4 + 1))
      where rslt1 = exprToTAC name state left
            (src1, n1) = snd rslt1
            (dstName1, n2) = makeTemp name n1
            rslt2 = exprToTAC name (src1, n2) right
            (src2, n3) = snd rslt2
            (dstName2, n4) = makeTemp name n3
            dst = TACVar dstName2
            endStr = "end." ++ show n4
    -- relational operators (probably should rewrite)
    (ASTBinary BoolEq left right) -> relationalToTAC name state BoolEq left right
    (ASTBinary BoolNeq left right) -> relationalToTAC name state BoolNeq left right
    (ASTBinary BoolGe left right) -> relationalToTAC name state BoolGe left right
    (ASTBinary BoolGeq left right) -> relationalToTAC name state BoolGeq left right
    (ASTBinary BoolLe left right) -> relationalToTAC name state BoolLe left right
    (ASTBinary BoolLeq left right) -> relationalToTAC name state BoolLeq left right
    -- arithmetic operators (single instruction)
    -- could move mul/div/mod from a function call to here
    (ASTBinary op left right) ->
      (fst rslt1 ++ fst rslt2 ++ [TACBinary op dst src1 src2], (dst, n4))
      where rslt1 = exprToTAC name state left
            (src1, n1) = snd rslt1
            (dstName1, n2) = makeTemp name n1
            rslt2 = exprToTAC name (src1, n2) right
            (src2, n3) = snd rslt2
            (dstName2, n4) = makeTemp name n3
            dst = TACVar dstName2 -- possible optimization: use dstName1
    (ASTAssign (Factor (ASTVar v)) right) -> 
      (fst rslt ++ [TACCopy dst src], (dst, n))
      where rslt = exprToTAC name state right
            (src, n) = snd rslt
            dst = TACVar v
    (ASTPostAssign (Factor (ASTVar v)) right) -> 
      ([TACCopy oldVal dst] ++ 
        fst rslt ++ 
        [TACCopy dst src], (oldVal, n2))
      where rslt = exprToTAC name state right
            (src, n1) = snd rslt
            (oldValName, n2) = makeTemp name n1
            oldVal = TACVar oldValName
            dst = TACVar v
    (Conditional condition left right) -> (
      rslt1 ++ 
      [TACCmp src1 (TACLit 0),
       TACCondJump CondE elseStr] ++
       rslt2 ++
       [TACCopy dst src2,
       TACJump endStr,
       TACLabel elseStr] ++
       rslt3 ++
      [TACCopy dst src3,
       TACLabel endStr], (dst, n6 + 1))
      where (rslt1, (src1, n1)) = exprToTAC name state condition
            (dstName1, n2) = makeTemp name n1
            (rslt2, (src2, n3)) = exprToTAC name (src1, n2) left
            (dstName2, n4) = makeTemp name n3
            elseStr = "else." ++ show n4
            (rslt3, (src3, n5)) = exprToTAC name (src1, n4 + 1) right
            (dstName3, n6) = makeTemp name n5
            dst = TACVar dstName3
            endStr = "end." ++ show n6