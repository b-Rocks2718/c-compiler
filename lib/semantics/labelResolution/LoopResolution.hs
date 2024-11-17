{-# LANGUAGE NamedFieldPuns #-}

module LoopResolution where

import Utils
import AST

data LabelType = Loop | Switch

data LoopLabelState = LoopLabelState {
  prevLoopLabel :: Maybe String,
  prevSwitchLabel :: Maybe String,
  lastLabel :: LabelType
}

labelLoops :: Declaration -> Result Declaration
labelLoops dclr =
  case dclr of
    FunDclr (FunctionDclr name type_ mStorage params body) -> do
      FunDclr . FunctionDclr name type_ mStorage params <$>
        evalStateT (liftMaybe (labelBlock name) body) (LoopLabelState Nothing Nothing Loop, 0)
    VarDclr _ -> pure dclr -- nothing to label

labelStmt :: String -> Stmt -> StateT (LoopLabelState, Int) Result Stmt
labelStmt name stmt = case stmt of
  BreakStmt _ -> do
    LoopLabelState {prevLoopLabel, prevSwitchLabel, lastLabel} <- getFst
    case lastLabel of
      Loop -> case prevLoopLabel of
        Nothing -> lift (Err "Semantics Error: Break statement outside loop/switch")
        Just _ -> return (BreakStmt prevLoopLabel)
      Switch -> case prevSwitchLabel of
        Nothing -> lift (Err "Semantics Error: Break statement outside loop/switch")
        Just _ -> return (BreakStmt prevSwitchLabel)
  ContinueStmt _ -> do
    LoopLabelState {prevLoopLabel} <- getFst
    case prevLoopLabel of
      Nothing -> lift (Err "Semantics Error: Continue statement outside loop")
      Just _ -> return (ContinueStmt prevLoopLabel)
  WhileStmt condition body _ -> do
    label <- makeUnique $ name ++ ".while"
    oldLabel@LoopLabelState {prevSwitchLabel} <- getFst
    putFst $ LoopLabelState (Just label) prevSwitchLabel Loop
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (WhileStmt condition labeledBody (pure label))
  DoWhileStmt body condition _ -> do
    label <- makeUnique $ name ++ ".doWhile"
    oldLabel@LoopLabelState {prevSwitchLabel} <- getFst
    putFst $ LoopLabelState (Just label) prevSwitchLabel Loop
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (DoWhileStmt labeledBody condition (pure label))
  ForStmt init_ condition end body _ -> do
    label <- makeUnique $ name ++ ".for"
    oldLabel@LoopLabelState {prevSwitchLabel} <- getFst
    putFst $ LoopLabelState (Just label) prevSwitchLabel Loop
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (ForStmt init_ condition end labeledBody (pure label))
  IfStmt condition left right -> do
    labeledLeft <- labelStmt name left
    labeledRight <- liftMaybe (labelStmt name) right
    return (IfStmt condition labeledLeft labeledRight)
  CompoundStmt block -> do
    labeledBlock <- labelBlock name block
    return (CompoundStmt labeledBlock)
  LabeledStmt s stmt' -> do
    labeledStmt <- labelStmt name stmt'
    return (LabeledStmt s labeledStmt)
  CaseStmt expr stmt' _ -> do
    LoopLabelState {prevSwitchLabel} <- getFst
    label <- case prevSwitchLabel of
      Nothing -> lift (Err "Semantics Error: Case statement outside switch")
      Just l -> return l
    labeledStmt <- labelStmt name stmt'
    n <- case expr of
      Lit n -> return n
      _ -> error "Compiler Error: Case has none constant expr"
    return (CaseStmt expr labeledStmt (Just $ label ++ "." ++ show (getConstInt n)))
  DefaultStmt stmt' _ -> do
    LoopLabelState {prevSwitchLabel} <- getFst
    label <- case prevSwitchLabel of
      Nothing -> lift (Err "Semantics Error: Default statement outside switch")
      Just l -> return l
    labeledStmt <- labelStmt name stmt'
    return (DefaultStmt labeledStmt (Just $ label ++ ".default"))
  SwitchStmt expr stmt' _ cases -> do
    label <- makeUnique $ name ++ ".switch"
    oldLabel@LoopLabelState {prevLoopLabel} <- getFst
    putFst $ LoopLabelState prevLoopLabel (Just label) Switch
    labeledBody <- labelStmt name stmt'
    putFst oldLabel
    return (SwitchStmt expr labeledBody (pure label) cases)
  -- might as well label return statements while we're here
  RetStmt expr _ -> return (RetStmt expr (Just name))
  _ -> return stmt

labelBlockItem :: String -> BlockItem -> StateT (LoopLabelState, Int) Result BlockItem
labelBlockItem name item = case item of
  StmtBlock stmt -> do
    rslt <- labelStmt name stmt
    return (StmtBlock rslt)
  DclrBlock dclr -> return (DclrBlock dclr)

labelBlock :: String -> Block -> StateT (LoopLabelState, Int) Result Block
labelBlock name (Block items) =  do
    labeledItems <- foldr (liftA2 (:) . labelBlockItem name)
      (return []) items
    return (Block labeledItems)