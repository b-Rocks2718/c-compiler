module TAC where

import Utils
import qualified TypedAST
import TypedAST(
  SymbolTable,
  IdentAttrs(..),
  IdentInit(..),
  StaticInit(..),
  Const_(..),
  isGlobal,
  getExprType
  )
import AST(
  Type_(..),
  BinOp(..),
  PostOp(..),
  CaseLabel(..),
  compoundOps,
  relationalOps, UnaryOp (BoolNot))
import ParserUtils(getCompoundOp)
import TACAST

-- initialize global counter here
progToTAC :: SymbolTable -> TypedAST.Prog -> (SymbolTable, Prog)
progToTAC symbols (TypedAST.Prog p) =
  let evalDclr d =
        evalState (fileScopeDclrToTAC symbols d) (tacDataInit symbols)
      rslt = evalDclr <$> p
      newSymbols = last $ fst <$> rslt
      instrs = concatMap snd rslt
  in (newSymbols, Prog ([Comment "Data Section:"] ++ concatMap symbolToTAC symbols ++
              [Comment "Code Section:"] ++ instrs))

fileScopeDclrToTAC :: SymbolTable -> TypedAST.Declaration -> TACState (SymbolTable, [TopLevel])
fileScopeDclrToTAC symbols dclr = case dclr of
  TypedAST.VarDclr (TypedAST.VariableDclr {}) -> return (symbols, []) -- returns empty list (will process later)
  TypedAST.FunDclr f -> do
    instrs <- funcToTAC f symbols -- returns singleton list
    symbols' <- getSymbols <$> get
    return (symbols', instrs)

-- takes an element of the symbol table, returns a global var or empty list
symbolToTAC :: (String, (Type_, IdentAttrs)) -> [TopLevel]
symbolToTAC (_, (FunType {}, _)) = []
symbolToTAC (name, (_, StaticAttr init_ global)) =
  case init_ of
    Initial (IntInit i) -> [StaticVar name global IntType (IntInit i)]
    Initial (UIntInit i) -> [StaticVar name global UIntType (UIntInit i)]
    Tentative -> [StaticVar name global IntType (IntInit 0)]
    NoInit -> []
symbolToTAC _ = []

funcToTAC :: TypedAST.FunctionDclr -> SymbolTable -> TACState [TopLevel]
funcToTAC (TypedAST.FunctionDclr name _ _ params mBody) symbols =
  case mBody of
    Just _ -> do
      body <- mBodyToTAC name mBody
      case lookup name symbols of
        Just (FunType _ _, attrs) ->
          return [Func name (TypedAST.isGlobal attrs) (paramTo <$> params) $
          body ++ [Return (makeConstant IntType 0)]]
        _ -> error "Compiler Error: missing or invalid symbol table entry"
    Nothing -> return []


mBodyToTAC :: String -> Maybe TypedAST.Block -> TACState [Instr]
mBodyToTAC name mBody = case mBody of
  Just body -> blockTo name body
  Nothing -> return []

paramTo :: TypedAST.VariableDclr -> String
paramTo (TypedAST.VariableDclr name _ _ _) = name

blockTo :: String -> TypedAST.Block -> TACState [Instr]
blockTo name (TypedAST.Block items) = do
  init_ <- get
  foldl' (blockItemsTo name) (state $ const ([], init_)) items

blockItemsTo :: String -> TACState [Instr] ->
    TypedAST.BlockItem -> TACState [Instr]
blockItemsTo name tacState item = case item of
  TypedAST.StmtBlock stmt -> do
    instrs <- tacState
    newInstrs <- stmtToTAC name stmt
    return (instrs ++ newInstrs)
  TypedAST.DclrBlock dclr -> do
    instrs <- tacState
    newInstrs <- localDclrTo name dclr
    return (instrs ++ newInstrs)

localDclrTo :: String -> TypedAST.Declaration -> TACState [Instr]
localDclrTo name dclr = case dclr of
  TypedAST.VarDclr v -> varDclrTo name v
  TypedAST.FunDclr f -> case f of
    (TypedAST.FunctionDclr _ _ _ _ Nothing) -> return []
    _ -> error "Compiler Error: Local function should have been found by now"
      -- local functions definitions should have been caught by now

varDclrTo :: String -> TypedAST.VariableDclr -> TACState [Instr]
varDclrTo name (TypedAST.VariableDclr varName type_ mStorage mExpr) = case mExpr of
  Just expr -> case mStorage of
    Just _ -> pure []
    Nothing -> exprToTAC name (TypedAST.Assign (TypedAST.Var varName type_) expr type_)
  Nothing -> pure []

stmtToTAC :: String -> TypedAST.Stmt -> TACState [Instr]
stmtToTAC name stmt = case stmt of
  (TypedAST.RetStmt expr) -> do
    instrs <- exprToTAC name expr
    dst <- getDst <$> get
    return (instrs ++ [Return dst])
  (TypedAST.ExprStmt expr) -> exprToTAC name expr
  (TypedAST.IfStmt condition left right) -> case right of
    Just stmt' -> ifElseToTAC name condition left stmt'
    Nothing -> ifToTAC name condition left
  (TypedAST.GoToStmt label) -> return [Jump label]
  (TypedAST.LabeledStmt label stmt') -> do
    instrs <- stmtToTAC name stmt'
    return $ Label label : instrs
  (TypedAST.CompoundStmt block) -> blockTo name block
  (TypedAST.BreakStmt mLabel) -> case mLabel of
    Just label -> return [Jump $ label ++ ".break"]
    Nothing -> error "Compiler Error: Loops should be labeled by now"
  (TypedAST.ContinueStmt mLabel) -> case mLabel of
    Just label -> return [Jump $ label ++ ".continue"]
    Nothing -> error "Compiler Error: Loops should be labeled by now"
  (TypedAST.DoWhileStmt body condition mLabel) -> doWhileTo name body condition mLabel
  (TypedAST.WhileStmt condition body mLabel) -> whileTo name condition body mLabel
  (TypedAST.ForStmt init_ condition end body mLabel) -> forToTAC name init_ condition end body mLabel
  (TypedAST.SwitchStmt expr stmt' mLabel cases) -> do
    let label = case mLabel of
          Just l -> l
          Nothing -> error "Compiler Error: Switch statement should be labeled by now"
    exprInstrs <- exprToTAC name expr
    casesInstrs <- casesTo label cases
    stmtInstrs <- stmtToTAC name stmt'
    return (exprInstrs ++ casesInstrs ++ stmtInstrs ++ [Label $ label ++ ".break"])
  (TypedAST.CaseStmt _ stmt' label) -> case label of
    Just l -> do
      stmtInstrs <- stmtToTAC name stmt'
      return (Label l : stmtInstrs)
    Nothing -> error "Compiler Error: Case statement should be labeled by now"
  (TypedAST.DefaultStmt stmt' label) -> case label of
    Just l -> do
      stmtInstrs <- stmtToTAC name stmt'
      return (Label l : stmtInstrs)
    Nothing -> error "Compiler Error: Default statement should be labeled by now"
  TypedAST.NullStmt -> return []

casesTo :: String -> Maybe [CaseLabel] -> TACState [Instr]
casesTo label mCases = case mCases of
  Nothing -> error "Compiler Error: cases should be collected by now"
  Just cases -> do
    instrs <- mapM (caseTo label) cases
    return (concat instrs ++ [Jump $ label ++ ".break"])

caseTo :: String -> CaseLabel -> TACState [Instr]
caseTo label (IntCase n) = do
  rslt <- getDst <$> get
  return [Cmp rslt (Constant (ConstInt n)), CondJump CondE $ label ++ "." ++ show n]
caseTo label DefaultCase = return [Jump $ label ++ ".default"]

doWhileTo :: String -> TypedAST.Stmt -> TypedAST.Expr -> Maybe String -> TACState [Instr]
doWhileTo name body condition mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "Compiler Error: loops should be labeled by now"
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- exprToTAC name condition
  rslt <- getDst <$> get
  return ( [Label $ label ++ ".start"] ++
    bodyInstrs ++
    [Label $ label ++ ".continue"] ++
    conditionInstrs ++
    [Cmp rslt (makeConstant IntType 0),
    CondJump CondNE $ label ++ ".start"] ++
    [Label $ label ++ ".break"])

whileTo :: String -> TypedAST.Expr -> TypedAST.Stmt -> Maybe String -> TACState [Instr]
whileTo name condition body mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "Compiler Error: loops should be labeled by now"
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- exprToTAC name condition
  rslt <- getDst <$> get
  return ([Label $ label ++ ".continue"] ++
    conditionInstrs ++
    [Cmp rslt (makeConstant IntType 0),
    CondJump CondE $ label ++ ".break"] ++
    bodyInstrs ++
    [Jump $ label ++ ".continue"] ++
    [Label $ label ++ ".break"])

forToTAC :: String -> TypedAST.ForInit -> Maybe TypedAST.Expr -> Maybe TypedAST.Expr ->
  TypedAST.Stmt -> Maybe String -> TACState [Instr]
forToTAC name init_ condition end body mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "Compiler Error: Loops should be labeled by now"
  initInstrs <- initToTAC name init_
  bodyInstrs <- stmtToTAC name body
  conditionInstrs <- case condition of
    Just c -> do
      cExprInstrs <- exprToTAC name c
      rslt <- getDst <$> get
      return (cExprInstrs ++
            [Cmp rslt (makeConstant IntType 0),
            CondJump CondE $ label ++ ".break"])
    Nothing -> pure []
  endInstrs <- case end of
    Just e -> exprToTAC name e
    Nothing -> pure []
  return (initInstrs ++
    [Label $ label ++ ".start"] ++
    conditionInstrs ++    bodyInstrs ++
    [Label $ label ++ ".continue"] ++
    endInstrs ++
    [Jump $ label ++ ".start"] ++
    [Label $ label ++ ".break"])

initToTAC :: String -> TypedAST.ForInit -> TACState [Instr]
initToTAC name init_ = case init_ of
  TypedAST.InitDclr d -> varDclrTo name d
  TypedAST.InitExpr e -> case e of
    Just expr -> exprToTAC name expr
    Nothing -> pure []

ifToTAC :: String -> TypedAST.Expr ->
    TypedAST.Stmt -> TACState [Instr]
ifToTAC name condition left = do
  rslt1 <- exprToTAC name condition
  src1 <- getDst <$> get
  rslt2 <- stmtToTAC name left
  n <- getN <$> get
  let endStr = name ++ ".end." ++ show n
  putN (n + 1)
  return (rslt1 ++
          [Cmp src1 (makeConstant IntType 0),
           CondJump CondE endStr] ++
           rslt2 ++
          [Label endStr])

ifElseToTAC :: String -> TypedAST.Expr ->
    TypedAST.Stmt -> TypedAST.Stmt -> TACState [Instr]
ifElseToTAC name condition left right = do
  rslt1 <- exprToTAC name condition
  src1 <- getDst <$> get
  rslt2 <- stmtToTAC name left
  n <- getN <$> get
  let elseStr = name ++ ".else." ++ show n
  putN (n + 1)
  rslt3 <- stmtToTAC name right
  n' <- getN <$> get
  let endStr = name ++ ".end." ++ show n'
  putN (n' + 1)
  return (rslt1 ++
    [Cmp src1 (makeConstant IntType 0),
     CondJump CondE elseStr] ++
     rslt2 ++
     [Jump endStr,
     Label elseStr] ++
     rslt3 ++
    [Label endStr])

argsToTAC :: String -> [TypedAST.Expr] -> TACState ([Instr], [Val])
argsToTAC name = foldl' (argsFold name) (return ([], []))

argsFold :: String -> TACState ([Instr], [Val]) ->
    TypedAST.Expr -> TACState ([Instr], [Val])
argsFold name oldState expr = do
  (instrs, srcs) <- oldState
  newInstrs <- exprToTAC name expr
  src <- getDst <$> get
  return (instrs ++ newInstrs, srcs ++ [src])

relationToCond :: BinOp -> Type_ -> Condition
relationToCond op type_ = case op of
  BoolEq -> CondE
  BoolNeq -> CondNE
  BoolGe -> case type_ of
    IntType -> CondG
    UIntType -> CondA
    _ -> error "Compiler Error: invalid type"
  BoolGeq -> case type_ of
    IntType -> CondGE
    UIntType -> CondAE
    _ -> error "Compiler Error: invalid type"
  BoolLe -> case type_ of
    IntType -> CondL
    UIntType -> CondB
    _ -> error "Compiler Error: invalid type"
  BoolLeq -> case type_ of
    IntType -> CondLE
    UIntType -> CondBE
    _ -> error "Compiler Error: invalid type"
  _ -> error "Compiler Error: not a relational condition"

relationalToTAC :: String -> BinOp -> TypedAST.Expr -> TypedAST.Expr -> Type_ -> TACState [Instr]
relationalToTAC name op left right type_ = do
  rslt1 <- exprToTAC name left
  src1 <- getDst <$> get
  rslt2 <- exprToTAC name right
  src2 <- getDst <$> get
  dst <- makeTemp name IntType -- relationals always return int
  n <- getN <$> get
  let endStr = name ++ ".end." ++ show n
  putDst dst
  putN (n + 1)
  return ([Copy dst (makeConstant IntType 1)] ++
          rslt1 ++ rslt2 ++
          [Cmp src1 src2,
          CondJump (relationToCond op type_) endStr,
          Copy dst (makeConstant IntType 0),
          Label endStr])

exprToTAC :: String -> TypedAST.Expr -> TACState [Instr]
exprToTAC name expr =
  case expr of
    -- short-circuiting operators
    (TypedAST.Binary BoolAnd left right type_) -> do
      rslt1 <- exprToTAC name left
      src1 <- getDst <$> get
      rslt2 <- exprToTAC name right
      src2 <- getDst <$> get
      dst <- makeTemp name type_
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putDst dst
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 0)] ++
        rslt1 ++
        [Cmp src1 (makeConstant IntType 0),
        CondJump CondE endStr] ++
        rslt2 ++
        [Cmp src2 (makeConstant IntType 0),
        CondJump CondE endStr,
        Copy dst (makeConstant IntType 1),
        Label endStr])
    (TypedAST.Binary BoolOr left right type_) -> do
      rslt1 <- exprToTAC name left
      src1 <- getDst <$> get
      rslt2 <- exprToTAC name right
      src2 <- getDst <$> get
      dst <- makeTemp name type_
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putDst dst
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 1)] ++
              rslt1 ++
              [Cmp src1 (makeConstant IntType 0),
              CondJump CondNE endStr] ++
              rslt2 ++
              [Cmp src2 (makeConstant IntType 0),
              CondJump CondNE endStr,
              Copy dst (makeConstant IntType 0),
              Label endStr])
    (TypedAST.Binary op left right type_) -> if op `elem` relationalOps
      then relationalToTAC name op left right (getExprType left)
      else if op `elem` compoundOps
        then do
          rslt1 <- exprToTAC name left
          src1 <- getDst <$> get
          rslt2 <- exprToTAC name right
          src2 <- getDst <$> get
          putDst src1 -- compound op stores result back in src1
          return (rslt1 ++ rslt2 ++ [Binary (getCompoundOp op) src1 src1 src2 type_])
      else do
        rslt1 <- exprToTAC name left
        src1 <- getDst <$> get
        --dst1 <- makeTemp name
        rslt2 <- exprToTAC name right
        src2 <- getDst <$> get
        dst2 <- makeTemp name type_ -- non compound op makes new variable for result
        putDst dst2 -- possible optimization: use dst1
        return (rslt1 ++ rslt2 ++ [Binary op dst2 src1 src2 type_])
    (TypedAST.Assign (TypedAST.Var v _) expr' _) -> do
      -- possible optimization: remove the Copy here, pass dst to expr function
      rslt <- exprToTAC name expr'
      src <- getDst <$> get
      let dst = Var v
      putDst dst
      return (rslt ++ [Copy dst src])
    (TypedAST.Assign {}) -> error "Compiler Error: missed invalid lvalue"
    (TypedAST.PostAssign (TypedAST.Var v _) op type_) -> do
      let src = Var v
      oldVal <- makeTemp name type_
      putDst oldVal
      let binOp = if op == PostInc then AddOp else SubOp
      return [Copy oldVal src, Binary binOp src src (makeConstant IntType 1) type_]
    (TypedAST.PostAssign {}) -> error "Compiler Error: missed invalid lvalue"
    (TypedAST.Conditional condition left right type_) -> do
      rslt1 <- exprToTAC name condition
      src1 <- getDst <$> get
      rslt2 <- exprToTAC name left
      src2 <- getDst <$> get
      n <- getN <$> get
      let elseStr = name ++ ".else." ++ show n
      putN (n + 1)
      rslt3 <- exprToTAC name right
      src3 <- getDst <$> get
      dst <- makeTemp name type_
      n' <- getN <$> get
      let endStr = name ++ ".end." ++ show n'
      putDst dst
      putN (n' + 1)
      return (rslt1 ++
        [Cmp src1 (makeConstant IntType 0),
         CondJump CondE elseStr] ++
         rslt2 ++
         [Copy dst src2,
         Jump endStr,
         Label elseStr] ++
         rslt3 ++
        [Copy dst src3,
         Label endStr])
    (TypedAST.Lit m _) -> do
      putDst (Constant m)
      return []
    (TypedAST.Unary BoolNot fctr _) -> do
      rslt1 <- exprToTAC name fctr
      src1 <- getDst <$> get
      dst <- makeTemp name IntType -- bool ops always return int
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putDst dst
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 1)] ++
              rslt1 ++
              [Cmp src1 (makeConstant IntType 0),
              CondJump CondE endStr,
              Copy dst (makeConstant IntType 0),
              Label endStr])
    (TypedAST.Unary op expr' type_) -> do
      rslt <- exprToTAC name expr'
      src <- getDst <$> get
      dst <- makeTemp name type_
      putDst dst
      return (rslt ++ [Unary op dst src])
    (TypedAST.Var v _) -> do
      putDst (Var v)
      return []
    (TypedAST.FunctionCall funcName args type_) -> do
      (rslts, srcs) <- argsToTAC name args
      dst <- makeTemp name type_
      putDst dst
      return (rslts ++ [Call funcName dst srcs])
    (TypedAST.Cast type_ expr') -> do
      rslt <- exprToTAC name expr'
      cast <-
        if type_ == getExprType expr' then
          return [] -- return rslt
        else do
          -- if typed had multiple sizes, this is where
          -- you'd truncate or extend
          -- right now, the only typed are 16 bit signed or unsigned
          src <- getDst <$> get
          dst <- makeTemp name type_
          putDst dst
          return [Copy dst src]
      return (rslt ++ cast)

-- utils

tacDataInit :: SymbolTable -> TACData
tacDataInit symbols = TACData (makeConstant IntType 0) symbols 0

-- create a unique temporary variable name
makeTemp :: String -> Type_ -> TACState Val
makeTemp name type_ = do
  n <- getN <$> get
  putN (n + 1)
  let varName = name ++ ".tmp." ++ show n
  -- add to symbol table
  symbols <- getSymbols <$> get
  putSymbols $ (varName, (type_, LocalAttr)) : symbols
  return (Var varName)

putDst :: Val -> TACState ()
putDst dst = do
  TACData _ symbols n <- get
  put (TACData dst symbols n)

putSymbols :: SymbolTable -> TACState ()
putSymbols symbols = do
  TACData dst _ n <- get
  put (TACData dst symbols n)

putN :: Int -> TACState ()
putN n = do
  TACData dst symbols _ <- get
  put (TACData dst symbols n)

makeConstant :: Type_ -> Int -> Val
makeConstant IntType = Constant . ConstInt
makeConstant UIntType = Constant . ConstUInt
makeConstant _ = error "Compiler Error: invalid constant"