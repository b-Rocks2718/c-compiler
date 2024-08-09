{-# LANGUAGE FlexibleContexts #-}

module TAC where

import Lexer
import Parser
import Semantics
import Data.List ( foldl' )
import Control.Monad.State

newtype TACProg  = TACProg TACFunc deriving (Show)
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

type TACState = State (TACVal, Int) [TACInstr]

instance Show TACFunc where
  show (TACFunc name instrs) =
    "TACFunc " ++ show name ++ ":\n" ++
    unlines (show <$> instrs)

progToTAC :: ASTProg -> TACProg
progToTAC (ASTProg f) = TACProg $ funcToTAC f

-- initialize global counter here
funcToTAC :: ASTFunc -> TACFunc
funcToTAC (ASTFunc name body) = TACFunc name $
  evalState (blockToTAC name body) (TACLit 0, 0) ++ [TACReturn (TACLit 0)]

blockToTAC :: String -> Block -> TACState
blockToTAC name (Block items) = do
  n <- getN
  foldl' (blockItemsToTAC name) (state $ const ([], (TACLit 0, n))) items

blockItemsToTAC :: String -> TACState ->
    BlockItem -> TACState
blockItemsToTAC name state item = case item of
  StmtBlock stmt -> do
    instrs <- state
    newInstrs <- stmtToTAC name stmt
    return (instrs ++ newInstrs)
  DclrBlock (ASTDclr varName mExpr) -> case mExpr of
    Just expr -> do
      instrs <- state
      newInstrs <- exprToTAC name (ASTAssign (Factor $ ASTVar varName) expr)
      return (instrs ++ newInstrs)
    Nothing -> state

stmtToTAC :: String -> ASTStmt -> TACState
stmtToTAC name stmt = case stmt of
  (RetStmt expr) -> do
    instrs <- exprToTAC name expr
    dst <- getVal
    return (instrs ++ [TACReturn dst])
  (ExprStmt expr) -> exprToTAC name expr
  (IfStmt condition left right) -> case right of
    Just stmt -> ifElseToTAC name condition left stmt
    Nothing -> ifToTAC name condition left
  (GoToStmt label) -> return [TACJump label]
  (LabeledStmt label stmt) -> do
    instrs <- stmtToTAC name stmt
    return $ TACLabel label : instrs
  (CompoundStmt block) -> blockToTAC name block
  NullStmt -> return []

ifToTAC :: String -> ASTExpr ->
    ASTStmt -> TACState
ifToTAC name condition left = do
  rslt1 <- exprToTAC name condition
  src1 <- getVal
  rslt2 <- stmtToTAC name left
  dst <- makeTemp name
  n <- getN
  let endStr = "end." ++ show n
  put (dst, n + 1)
  return (rslt1 ++
          [TACCmp src1 (TACLit 0),
           TACCondJump CondE endStr] ++
           rslt2 ++
          [TACLabel endStr])

ifElseToTAC :: String -> ASTExpr ->
    ASTStmt -> ASTStmt -> TACState
ifElseToTAC name condition left right = do
  rslt1 <- exprToTAC name condition
  src1 <- getVal
  rslt2 <- stmtToTAC name left
  n <- getN
  let elseStr = "else." ++ show n
  putN (n + 1)
  rslt3 <- stmtToTAC name right
  dst <- makeTemp name
  n' <- getN
  let endStr = "end." ++ show n'
  put (dst, n' + 1)
  return (rslt1 ++
    [TACCmp src1 (TACLit 0),
     TACCondJump CondE elseStr] ++
     rslt2 ++
     [TACJump endStr,
     TACLabel elseStr] ++
     rslt3 ++
    [TACLabel endStr])

-- create a temporary variable name
makeTemp :: String -> State (TACVal, Int) TACVal
makeTemp name = do
  n <- getN
  putN (n + 1)
  return (TACVar $ name ++ ".tmp." ++ show n)

getVal :: MonadState (a, b) m => m a
getVal = do
  gets fst

putVal :: MonadState (a, b) m => a -> m ()
putVal val = do
  (_, n) <- get
  put (val, n)

getN :: MonadState (a, b) m => m b
getN = do
  gets snd

putN :: MonadState (a, b) m => b -> m ()
putN n = do
  (val, _) <- get
  put (val, n)

factorToTAC :: String -> Factor -> TACState
factorToTAC name factor =
  case factor of
    (ASTLit m) -> do
      putVal (TACLit m)
      return []
    (ASTUnary op expr) -> do
      rslt <- factorToTAC name expr
      src <- getVal
      dst <- makeTemp name
      putVal dst
      return (rslt ++ [TACUnary op dst src])
    (FactorExpr e) -> exprToTAC name e
    (ASTVar v) -> do
      putVal (TACVar v)
      return []

relationToCond :: BinOp -> Condition
relationToCond op = case op of
  BoolEq -> CondE
  BoolNeq -> CondNE
  BoolGe -> CondG
  BoolGeq -> CondGE
  BoolLe -> CondL
  BoolLeq -> CondLE

relationalToTAC :: String -> BinOp -> ASTExpr -> ASTExpr -> TACState
relationalToTAC name op left right = do
  rslt1 <- exprToTAC name left
  src1 <- getVal
  rslt2 <- exprToTAC name right
  src2 <- getVal
  dst <- makeTemp name
  n <- getN
  let endStr = "end." ++ show n
  put (dst, n + 1)
  return ([TACCopy dst (TACLit 1)] ++
          rslt1 ++ rslt2 ++
          [TACCmp src1 src2,
          TACCondJump (relationToCond op) endStr,
          TACCopy dst (TACLit 0),
          TACLabel endStr])

exprToTAC :: String -> ASTExpr -> TACState
exprToTAC name expr =
  case expr of
    (Factor f) -> factorToTAC name f
    -- short-circuiting operators
    (ASTBinary BoolAnd left right) -> do
      rslt1 <- exprToTAC name left
      src1 <- getVal
      rslt2 <- exprToTAC name right
      src2 <- getVal
      dst <- makeTemp name
      n <- getN
      let endStr = "end." ++ show n
      put (dst, n + 1)
      return ([TACCopy dst (TACLit 0)] ++
        rslt1 ++
        [TACCmp src1 (TACLit 0),
        TACCondJump CondE endStr] ++
        rslt2 ++
        [TACCmp src2 (TACLit 0),
        TACCondJump CondE endStr,
        TACCopy dst (TACLit 1),
        TACLabel endStr])
    (ASTBinary BoolOr left right) -> do
      rslt1 <- exprToTAC name left
      src1 <- getVal
      rslt2 <- exprToTAC name right
      src2 <- getVal
      dst <- makeTemp name
      n <- getN
      let endStr = "end." ++ show n
      put (dst, n + 1)
      return ([TACCopy dst (TACLit 1)] ++
              rslt1 ++
              [TACCmp src1 (TACLit 0),
              TACCondJump CondNE endStr] ++
              rslt2 ++
              [TACCmp src2 (TACLit 0),
              TACCondJump CondNE endStr,
              TACCopy dst (TACLit 0),
              TACLabel endStr])
    -- relational operators (probably should rewrite)
    (ASTBinary BoolEq left right) -> relationalToTAC name BoolEq left right
    (ASTBinary BoolNeq left right) -> relationalToTAC name BoolNeq left right
    (ASTBinary BoolGe left right) -> relationalToTAC name BoolGe left right
    (ASTBinary BoolGeq left right) -> relationalToTAC name BoolGeq left right
    (ASTBinary BoolLe left right) -> relationalToTAC name BoolLe left right
    (ASTBinary BoolLeq left right) -> relationalToTAC name BoolLeq left right
    -- arithmetic operators (single instruction)
    -- could move mul/div/mod from a function call to here
    (ASTBinary op left right) -> do
      rslt1 <- exprToTAC name left
      src1 <- getVal
      --dst1 <- makeTemp name
      rslt2 <- exprToTAC name right
      src2 <- getVal
      dst2 <- makeTemp name -- possible optimization: use dst1
      putVal dst2
      return (rslt1 ++ rslt2 ++ [TACBinary op dst2 src1 src2])
    (ASTAssign (Factor (ASTVar v)) right) -> do
      rslt <- exprToTAC name right
      src <- getVal
      let dst = TACVar v
      putVal dst
      return (rslt ++ [TACCopy dst src])
    (ASTPostAssign (Factor (ASTVar v)) right) -> do
      rslt <- exprToTAC name right
      src <- getVal
      oldVal <- makeTemp name
      let dst = TACVar v
      putVal oldVal
      return ([TACCopy oldVal dst] ++ rslt ++ [TACCopy dst src])
    (Conditional condition left right) -> do
      rslt1 <- exprToTAC name condition
      src1 <- getVal
      rslt2 <- exprToTAC name left
      src2 <- getVal
      n <- getN
      let elseStr = "else." ++ show n
      rslt3 <- exprToTAC name right
      src3 <- getVal
      dst <- makeTemp name
      n' <- getN
      let endStr = "end." ++ show n'
      put (dst, n' + 1)
      return (rslt1 ++
        [TACCmp src1 (TACLit 0),
         TACCondJump CondE elseStr] ++
         rslt2 ++
         [TACCopy dst src2,
         TACJump endStr,
         TACLabel elseStr] ++
         rslt3 ++
        [TACCopy dst src3,
         TACLabel endStr])