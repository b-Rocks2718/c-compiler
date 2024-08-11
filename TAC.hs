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
  n <- getSnd
  foldl' (blockItemsToTAC name) (state $ const ([], (TACLit 0, n))) items

blockItemsToTAC :: String -> TACState ->
    BlockItem -> TACState
blockItemsToTAC name state item = case item of
  StmtBlock stmt -> do
    instrs <- state
    newInstrs <- stmtToTAC name stmt
    return (instrs ++ newInstrs)
  DclrBlock dclr -> do
    instrs <- state
    newInstrs <- dclrToTAC name dclr
    return (instrs ++ newInstrs)

dclrToTAC :: String -> ASTDclr -> TACState
dclrToTAC name (ASTDclr varName mExpr) = case mExpr of
  Just expr -> exprToTAC name (ASTAssign (Factor $ ASTVar varName) expr)
  Nothing -> pure []

stmtToTAC :: String -> ASTStmt -> TACState
stmtToTAC name stmt = case stmt of
  (RetStmt expr) -> do
    instrs <- exprToTAC name expr
    dst <- getFst
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
  (BreakStmt mLabel) -> case mLabel of
    Just label -> return [TACJump $ label ++ ".break"]
    Nothing -> error "Critical Error"
  (ContinueStmt mLabel) -> case mLabel of
    Just label -> return [TACJump $ label ++ ".continue"]
    Nothing -> error "Critical Error"
  (DoWhileStmt body condition mLabel) -> doWhileToTAC name body condition mLabel
  (WhileStmt condition body mLabel) -> whileToTAC name condition body mLabel
  (ForStmt init condition end body mLabel) -> forToTAC name init condition end body mLabel
  NullStmt -> return []

doWhileToTAC :: String -> ASTStmt -> ASTExpr -> Maybe String -> TACState
doWhileToTAC name body condition mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "semantic analysis? more like semantic analySUS"
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- exprToTAC name condition
  rslt <- getFst
  return ( [TACLabel $ label ++ ".start"] ++
    bodyInstrs ++ 
    [TACLabel $ label ++ ".continue"] ++
    conditionInstrs ++
    [TACCmp rslt (TACLit 0), 
    TACCondJump CondNE $ label ++ ".start"] ++
    [TACLabel $ label ++ ".break"])

whileToTAC :: String -> ASTExpr -> ASTStmt -> Maybe String -> TACState
whileToTAC name condition body mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "semantic analysis? more like semantic analySUS"
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- exprToTAC name condition
  rslt <- getFst
  return ([TACLabel $ label ++ ".continue"] ++
    conditionInstrs ++ 
    [TACCmp rslt (TACLit 0),
    TACCondJump CondE $ label ++ ".break"] ++
    bodyInstrs ++
    [TACJump $ label ++ ".continue"] ++
    [TACLabel $ label ++ ".break"])

forToTAC :: String -> ForInit -> Maybe ASTExpr -> Maybe ASTExpr -> 
  ASTStmt -> Maybe String -> TACState
forToTAC name init condition end body mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "semantic analysis? more like semantic analySUS"
  initInstrs <- initToTAC name init
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- case condition of 
    Just c -> do
      cExprInstrs <- exprToTAC name c
      rslt <- getFst
      return (cExprInstrs ++ 
            [TACCmp rslt (TACLit 0),
            TACCondJump CondE $ label ++ ".break"])
    Nothing -> pure []
  endInstrs <- case end of 
    Just e -> exprToTAC name e
    Nothing -> pure []
  return (initInstrs ++
    [TACLabel $ label ++ ".start"] ++
    conditionInstrs ++
    bodyInstrs ++ 
    [TACLabel $ label ++ ".continue"] ++
    endInstrs ++
    [TACJump $ label ++ ".start"] ++
    [TACLabel $ label ++ ".break"])

initToTAC :: String -> ForInit -> TACState
initToTAC name init = case init of
  InitDclr d -> dclrToTAC name d
  InitExpr e -> case e of
    Just expr -> exprToTAC name expr
    Nothing -> pure []

ifToTAC :: String -> ASTExpr ->
    ASTStmt -> TACState
ifToTAC name condition left = do
  rslt1 <- exprToTAC name condition
  src1 <- getFst
  rslt2 <- stmtToTAC name left
  dst <- makeTemp name
  n <- getSnd
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
  src1 <- getFst
  rslt2 <- stmtToTAC name left
  n <- getSnd
  let elseStr = "else." ++ show n
  putSnd (n + 1)
  rslt3 <- stmtToTAC name right
  dst <- makeTemp name
  n' <- getSnd
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
  n <- getSnd
  putSnd (n + 1)
  return (TACVar $ name ++ ".tmp." ++ show n)

factorToTAC :: String -> Factor -> TACState
factorToTAC name factor =
  case factor of
    (ASTLit m) -> do
      putFst (TACLit m)
      return []
    (ASTUnary op expr) -> do
      rslt <- factorToTAC name expr
      src <- getFst
      dst <- makeTemp name
      putFst dst
      return (rslt ++ [TACUnary op dst src])
    (FactorExpr e) -> exprToTAC name e
    (ASTVar v) -> do
      putFst (TACVar v)
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
  src1 <- getFst
  rslt2 <- exprToTAC name right
  src2 <- getFst
  dst <- makeTemp name
  n <- getSnd
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
      src1 <- getFst
      rslt2 <- exprToTAC name right
      src2 <- getFst
      dst <- makeTemp name
      n <- getSnd
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
      src1 <- getFst
      rslt2 <- exprToTAC name right
      src2 <- getFst
      dst <- makeTemp name
      n <- getSnd
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
      src1 <- getFst
      --dst1 <- makeTemp name
      rslt2 <- exprToTAC name right
      src2 <- getFst
      dst2 <- makeTemp name -- possible optimization: use dst1
      putFst dst2
      return (rslt1 ++ rslt2 ++ [TACBinary op dst2 src1 src2])
    (ASTAssign (Factor (ASTVar v)) right) -> do
      rslt <- exprToTAC name right
      src <- getFst
      let dst = TACVar v
      putFst dst
      return (rslt ++ [TACCopy dst src])
    (ASTPostAssign (Factor (ASTVar v)) right) -> do
      rslt <- exprToTAC name right
      src <- getFst
      oldVal <- makeTemp name
      let dst = TACVar v
      putFst oldVal
      return ([TACCopy oldVal dst] ++ rslt ++ [TACCopy dst src])
    (Conditional condition left right) -> do
      rslt1 <- exprToTAC name condition
      src1 <- getFst
      rslt2 <- exprToTAC name left
      (src2, n) <- get
      let elseStr = "else." ++ show n
      rslt3 <- exprToTAC name right
      src3 <- getFst
      dst <- makeTemp name
      n' <- getSnd
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