module SwitchResolution where

import Utils
import AST

-- TODO: test nested switch statements

resolveSwitches :: Declaration -> Result Declaration
resolveSwitches dclr = 
  case dclr of 
    FunDclr (FunctionDclr name type_ mStorage params body) -> do
      FunDclr . FunctionDclr name type_ mStorage params <$>
        evalStateT (liftMaybe collectCaseBlock body) []
    VarDclr _ -> pure dclr

collectCaseBlock :: Block -> StateT [CaseLabel] Result Block
collectCaseBlock (Block items) =  do
    -- BlockItem ->
    --  StateT (Maybe String, Int) Result [BlockItem] ->
    --  StateT (Maybe String, Int) Result [BlockItem]
    labeledItems <- foldr (liftA2 (:) . collectCaseBlockItem)
      (return []) items
    return (Block labeledItems)

collectCaseBlockItem :: BlockItem -> StateT [CaseLabel] Result BlockItem
collectCaseBlockItem item = case item of
  StmtBlock stmt -> do
    rslt <- collectCasesStmt stmt
    return (StmtBlock rslt)
  DclrBlock dclr -> return (DclrBlock dclr)

collectCasesStmt :: Stmt -> StateT [CaseLabel] Result Stmt
collectCasesStmt stmt = case stmt of
  WhileStmt condition body label -> do
    newBody <- collectCasesStmt body
    return (WhileStmt condition newBody label)
  DoWhileStmt body condition label -> do
    newBody <- collectCasesStmt body
    return (DoWhileStmt newBody condition label)
  ForStmt init_ condition end body label -> do
    newBody <- collectCasesStmt body
    return (ForStmt init_ condition end newBody label)
  IfStmt condition left right -> do
    newLeft <- collectCasesStmt left
    newRight <- liftMaybe collectCasesStmt right
    return (IfStmt condition newLeft newRight)
  CompoundStmt block -> do
    newBlock <- collectCaseBlock block
    return (CompoundStmt newBlock)
  LabeledStmt s stmt' -> do
    newStmt <- collectCasesStmt stmt'
    return (LabeledStmt s newStmt)
  CaseStmt expr stmt' label -> do
    cases <- get
    case expr of
      Lit (ConstInt n) -> if IntCase n `elem` cases
        then lift (Err "Semantics Error: Duplicate cases")
        else put $ cases ++ [IntCase n]
      Lit (ConstUInt n) -> if IntCase n `elem` cases
        then lift (Err "Semantics Error: Duplicate cases")
        else put $ cases ++ [IntCase n]
      _ -> error "Compiler Error: Case expr was not converted to lit"
    newStmt <- collectCasesStmt stmt'
    return (CaseStmt expr newStmt label)
  DefaultStmt stmt' label -> do
    cases <- get
    if DefaultCase `elem` cases
      then lift (Err "Semantics Error: Duplicate default cases")
      else put $ cases ++ [DefaultCase]
    newStmt <- collectCasesStmt stmt'
    return (DefaultStmt newStmt label)
  SwitchStmt expr stmt' label _ -> do
    oldCases <- get
    put []
    newStmt <- collectCasesStmt stmt'
    rslt <- get
    put oldCases
    return (SwitchStmt expr newStmt label (Just rslt))
  _ -> return stmt