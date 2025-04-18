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
import Typechecking (
  isArithmeticType,
  isPointerType
  )
import AST(
  Type_(..),
  BinOp(..),
  PostOp(..),
  CaseLabel(..),
  compoundOps,
  relationalOps, UnaryOp (BoolNot))
import ParserUtils(getCompoundOp, typeSize, isSigned)
import TACAST
import TACUtils

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
symbolToTAC (name, (type_, StaticAttr init_ global)) =
  case init_ of
    Tentative -> [StaticVar name global IntType [ZeroInit (typeSize type_)]]
    NoInit -> []
    Initial vs -> [StaticVar name global IntType vs]
symbolToTAC _ = []

funcToTAC :: TypedAST.FunctionDclr -> SymbolTable -> TACState [TopLevel]
funcToTAC (TypedAST.FunctionDclr name _ _ params mBody) symbols =
  case mBody of
    Just _ -> do
      body <- mBodyToTAC name mBody
      case lookup name symbols of
        Just (FunType _ _, attrs) ->
          return [Func name (TypedAST.isGlobal attrs) (paramToTAC <$> params) $
          body ++ [Return (makeConstant IntType 0)]]
        _ -> error "Compiler Error: missing or invalid symbol table entry"
    Nothing -> return []

mBodyToTAC :: String -> Maybe TypedAST.Block -> TACState [Instr]
mBodyToTAC name mBody = case mBody of
  Just body -> blockToTAC name body
  Nothing -> return []

paramToTAC :: TypedAST.VariableDclr -> String
paramToTAC (TypedAST.VariableDclr name _ _ _) = name

blockToTAC :: String -> TypedAST.Block -> TACState [Instr]
blockToTAC name (TypedAST.Block items) = do
  init_ <- get
  foldl' (blockItemsToTAC name) (state $ const ([], init_)) items

blockItemsToTAC :: String -> TACState [Instr] ->
    TypedAST.BlockItem -> TACState [Instr]
blockItemsToTAC name tacState item = case item of
  TypedAST.StmtBlock stmt -> do
    instrs <- tacState
    newInstrs <- stmtToTAC name stmt
    return (instrs ++ newInstrs)
  TypedAST.DclrBlock dclr -> do
    instrs <- tacState
    newInstrs <- localDclrToTAC name dclr
    return (instrs ++ newInstrs)

localDclrToTAC :: String -> TypedAST.Declaration -> TACState [Instr]
localDclrToTAC name dclr = case dclr of
  TypedAST.VarDclr v -> varDclrToTAC name v
  TypedAST.FunDclr f -> case f of
    (TypedAST.FunctionDclr _ _ _ _ Nothing) -> return []
    _ -> error "Compiler Error: Local function should have been found by now"
      -- local functions definitions should have been caught by now

varDclrToTAC :: String -> TypedAST.VariableDclr -> TACState [Instr]
varDclrToTAC name (TypedAST.VariableDclr varName type_ mStorage mExpr) = case mExpr of
  Just init_ ->
    case init_ of
      TypedAST.SingleInit expr _ -> case mStorage of
        Just _ -> pure []
        Nothing -> fst <$> exprToTACConvert name
          (TypedAST.Assign (TypedAST.Var varName type_) expr type_)
      TypedAST.CompoundInit inits arrType -> arrayInitTAC name varName inits arrType 0
  Nothing -> pure []

arrayInitTAC :: String -> String -> [TypedAST.VarInit] -> Type_ -> Int -> TACState [Instr]
arrayInitTAC name vName inits type_ n = do
  symbols <- gets getSymbols
  putSymbols $ (vName, (type_, LocalAttr)) : symbols
  let dst = Var vName
  case inits of
    x : xs -> do
      case x of
        TypedAST.SingleInit expr inner -> do
          instrs <- arrayInitTAC name vName xs type_ (n + typeSize inner)
          (instrs2, rslt) <- exprToTAC name expr
          let src = getVal rslt
          return $ instrs2 ++ (CopyToOffset dst src (n * typeSize inner) : instrs)
        TypedAST.CompoundInit comp inner -> do
          instrs <- arrayInitTAC name vName xs type_ (n + typeSize inner)
          instrs2 <- arrayInitTAC name vName comp inner n
          return $ instrs2 ++  instrs
    [] -> return []

stmtToTAC :: String -> TypedAST.Stmt -> TACState [Instr]
stmtToTAC name stmt = case stmt of
  (TypedAST.RetStmt expr) -> do
    (instrs, dst) <- exprToTACConvert name expr
    return (instrs ++ [Return dst])
  (TypedAST.ExprStmt expr) -> fst <$> exprToTACConvert name expr
  (TypedAST.IfStmt condition left right) -> case right of
    Just stmt' -> ifElseToTAC name condition left stmt'
    Nothing -> ifToTAC name condition left
  (TypedAST.GoToStmt label) -> return [Jump label]
  (TypedAST.LabeledStmt label stmt') -> do
    instrs <- stmtToTAC name stmt'
    return $ Label label : instrs
  (TypedAST.CompoundStmt block) -> blockToTAC name block
  (TypedAST.BreakStmt mLabel) -> case mLabel of
    Just label -> return [Jump $ label ++ ".break"]
    Nothing -> error "Compiler Error: Loops should be labeled by now"
  (TypedAST.ContinueStmt mLabel) -> case mLabel of
    Just label -> return [Jump $ label ++ ".continue"]
    Nothing -> error "Compiler Error: Loops should be labeled by now"
  (TypedAST.DoWhileStmt body condition mLabel) -> doWhileToTAC name body condition mLabel
  (TypedAST.WhileStmt condition body mLabel) -> whileToTAC name condition body mLabel
  (TypedAST.ForStmt init_ condition end body mLabel) -> forToTAC name init_ condition end body mLabel
  (TypedAST.SwitchStmt expr stmt' mLabel cases) -> do
    let label = case mLabel of
          Just l -> l
          Nothing -> error "Compiler Error: Switch statement should be labeled by now"
    (exprInstrs, dst) <- exprToTACConvert name expr
    casesInstrs <- casesToTAC label cases dst
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

casesToTAC :: String -> Maybe [CaseLabel] -> Val -> TACState [Instr]
casesToTAC label mCases rslt = case mCases of
  Nothing -> error "Compiler Error: cases should be collected by now"
  Just cases -> do
    instrs <- mapM (caseToTAC label rslt) cases
    return (concat instrs ++ [Jump $ label ++ ".break"])

caseToTAC :: String -> Val -> CaseLabel -> TACState [Instr]
caseToTAC label rslt (IntCase n) = do
  return [Cmp rslt (Constant (ConstInt n)), CondJump CondE $ label ++ "." ++ show n]
caseToTAC label _ DefaultCase = return [Jump $ label ++ ".default"]

doWhileToTAC :: String -> TypedAST.Stmt -> TypedAST.Expr -> Maybe String -> TACState [Instr]
doWhileToTAC name body condition mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "Compiler Error: loops should be labeled by now"
  bodyInstrs <- stmtToTAC name body
  (conditionInstrs, rslt) <- exprToTACConvert name condition
  return ( [Label $ label ++ ".start"] ++
    bodyInstrs ++
    [Label $ label ++ ".continue"] ++
    conditionInstrs ++
    [Cmp rslt (makeConstant IntType 0),
    CondJump CondNE $ label ++ ".start"] ++
    [Label $ label ++ ".break"])

whileToTAC :: String -> TypedAST.Expr -> TypedAST.Stmt -> Maybe String -> TACState [Instr]
whileToTAC name condition body mLabel = do
  let label = case mLabel of
        Just x -> x
        Nothing -> error "Compiler Error: loops should be labeled by now"
  bodyInstrs <- stmtToTAC name body
  (conditionInstrs, rslt) <- exprToTACConvert name condition
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
      (cExprInstrs, rslt) <- exprToTACConvert name c
      return (cExprInstrs ++
            [Cmp rslt (makeConstant IntType 0),
            CondJump CondE $ label ++ ".break"])
    Nothing -> pure []
  (endInstrs, _) <- case end of
    Just e -> exprToTAC name e
    Nothing -> pure ([], PlainOperand $ Constant $ ConstInt 0)
  return (initInstrs ++
    [Label $ label ++ ".start"] ++
    conditionInstrs ++    bodyInstrs ++
    [Label $ label ++ ".continue"] ++
    endInstrs ++
    [Jump $ label ++ ".start"] ++
    [Label $ label ++ ".break"])

initToTAC :: String -> TypedAST.ForInit -> TACState [Instr]
initToTAC name init_ = case init_ of
  TypedAST.InitDclr d -> varDclrToTAC name d
  TypedAST.InitExpr e -> case e of
    Just expr -> fst <$> exprToTAC name expr
    Nothing -> pure []

ifToTAC :: String -> TypedAST.Expr ->
    TypedAST.Stmt -> TACState [Instr]
ifToTAC name condition left = do
  (rslt1, src1) <- exprToTACConvert name condition
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
  (rslt1, src1) <- exprToTACConvert name condition
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
  (newInstrs, src) <- exprToTACConvert name expr
  return (instrs ++ newInstrs, srcs ++ [src])

relationToCond :: BinOp -> Type_ -> Condition
relationToCond op type_ = case op of
  BoolEq -> CondE
  BoolNeq -> CondNE
  BoolGe -> if isSigned type_ then CondG else CondA
  BoolGeq -> if isSigned type_ then CondGE else CondAE
  BoolLe -> if isSigned type_ then CondL else CondB
  BoolLeq -> if isSigned type_ then CondLE else CondBE
  _ -> error "Compiler Error: not a relational condition"

relationalToTAC :: String -> BinOp -> TypedAST.Expr -> TypedAST.Expr -> Type_ -> TACState ([Instr], ExprResult)
relationalToTAC name op left right type_ = do
  (rslt1, src1) <- exprToTACConvert name left
  (rslt2, src2) <- exprToTACConvert name right
  dst <- makeTemp name IntType -- relationals always return int
  n <- getN <$> get
  let endStr = name ++ ".end." ++ show n
  putN (n + 1)
  return ([Copy dst (makeConstant IntType 1)] ++
          rslt1 ++ rslt2 ++
          [Cmp src1 src2,
          CondJump (relationToCond op type_) endStr,
          Copy dst (makeConstant IntType 0),
          Label endStr], PlainOperand dst)

exprToTACConvert :: String -> TypedAST.Expr -> TACState ([Instr], Val)
exprToTACConvert name expr = do
  (instrs, rslt) <- exprToTAC name expr
  case rslt of
    PlainOperand val -> return (instrs, val)
    DereferencedPointer ptr -> do
      dst <- makeTemp name (getExprType expr)
      return (instrs ++ [Load dst ptr], dst)

exprToTAC :: String -> TypedAST.Expr -> TACState ([Instr], ExprResult)
exprToTAC name expr =
  case expr of
    -- short-circuiting operators
    (TypedAST.Binary BoolAnd left right type_) -> do
      (rslt1, src1) <- exprToTACConvert name left
      (rslt2, src2) <- exprToTACConvert name right
      dst <- makeTemp name type_
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 0)] ++
        rslt1 ++
        [Cmp src1 (makeConstant IntType 0),
        CondJump CondE endStr] ++
        rslt2 ++
        [Cmp src2 (makeConstant IntType 0),
        CondJump CondE endStr,
        Copy dst (makeConstant IntType 1),
        Label endStr], PlainOperand dst)
    (TypedAST.Binary BoolOr left right type_) -> do
      (rslt1, src1) <- exprToTACConvert name left
      (rslt2, src2) <- exprToTACConvert name right
      dst <- makeTemp name type_
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 1)] ++
              rslt1 ++
              [Cmp src1 (makeConstant IntType 0),
              CondJump CondNE endStr] ++
              rslt2 ++
              [Cmp src2 (makeConstant IntType 0),
              CondJump CondNE endStr,
              Copy dst (makeConstant IntType 0),
              Label endStr], PlainOperand dst)
    (TypedAST.Binary AddOp left right type_) ->
      if isArithmeticType (getExprType left) && isArithmeticType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        return (rslt1 ++ rslt2 ++ [Binary AddOp dst2 src1 src2 type_], PlainOperand dst2)
      else if isArithmeticType (getExprType left) && isPointerType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_
        dst3 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        let refType = getRefType $ getExprType right
        let leftType = getExprType left
        return (rslt1 ++ rslt2 ++
          [ Binary MulOp dst2 src1 (makeConstant leftType $ typeSize refType) leftType,
            Binary AddOp dst3 dst2 src2 type_], PlainOperand dst3)
      else if isPointerType (getExprType left) && isArithmeticType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_
        dst3 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        let refType = getRefType $ getExprType left
        let rightType = getExprType right
        return (rslt1 ++ rslt2 ++
          [ Binary MulOp dst2 src2 (makeConstant rightType $ typeSize refType) rightType,
            Binary AddOp dst3 src1 dst2 type_], PlainOperand dst3)
      else return $ error "Compiler Error: invalid add made it past typechecking"
    (TypedAST.Binary SubOp left right type_) -> do
      if isArithmeticType (getExprType left) && isArithmeticType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        return (rslt1 ++ rslt2 ++ [Binary SubOp dst2 src1 src2 type_], PlainOperand dst2)
      else if isPointerType (getExprType left) && isArithmeticType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_
        dst3 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        let refType = getRefType $ getExprType left
        let rightType = getExprType right
        return (rslt1 ++ rslt2 ++
          [ Binary MulOp dst2 src2 (makeConstant rightType $ typeSize refType) rightType,
            Binary SubOp dst3 src1 dst2 type_], PlainOperand dst3)
      else return $ error "Compiler Error: invalid add made it past typechecking"
    (TypedAST.Binary op left right type_) -> if op `elem` relationalOps
      then relationalToTAC name op left right (getExprType left)
      else if op `elem` compoundOps
        then do
          (rslt1, src1) <- exprToTACConvert name left
          (rslt2, src2) <- exprToTACConvert name right
          -- compound op stores result back in src1
          return (rslt1 ++ rslt2 ++ [Binary (getCompoundOp op) src1 src1 src2 type_], PlainOperand src1)
      else do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_ -- non compound op makes new variable for result
        -- possible optimization: return dst1
        return (rslt1 ++ rslt2 ++ [Binary op dst2 src1 src2 type_], PlainOperand dst2)
    (TypedAST.Assign left right _) -> do
      -- possible optimization: remove the Copy here, pass dst to expr function
      (rslt1, lval) <- exprToTAC name left
      (rslt2, rval) <- exprToTACConvert name right
      case lval of
        PlainOperand obj ->
          return (rslt1 ++ rslt2 ++ [Copy obj rval], lval)
        DereferencedPointer ptr ->
          return (rslt1 ++ rslt2 ++ [Store ptr rval], PlainOperand rval)
    (TypedAST.PostAssign (TypedAST.Var v _) op type_) -> do
      let src = Var v
      oldVal <- makeTemp name type_
      let binOp = if op == PostInc then AddOp else SubOp
      return ([Copy oldVal src,
              Binary binOp src src (makeConstant IntType 1) type_],
              PlainOperand oldVal)
    (TypedAST.PostAssign {}) -> error "Compiler Error: missed invalid lvalue"
    (TypedAST.Conditional condition left right type_) -> do
      (rslt1, src1) <- exprToTACConvert name condition
      (rslt2, src2) <- exprToTACConvert name left
      n <- getN <$> get
      let elseStr = name ++ ".else." ++ show n
      putN (n + 1)
      (rslt3, src3) <- exprToTACConvert name right
      dst <- makeTemp name type_
      n' <- getN <$> get
      let endStr = name ++ ".end." ++ show n'
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
         Label endStr], PlainOperand dst)
    (TypedAST.Lit m _) -> return ([], PlainOperand $ Constant m)
    (TypedAST.Unary BoolNot fctr _) -> do
      (rslt1, src1) <- exprToTACConvert name fctr
      dst <- makeTemp name IntType -- bool ops always return int
      n <- getN <$> get
      let endStr = name ++ ".end." ++ show n
      putN (n + 1)
      return ([Copy dst (makeConstant IntType 1)] ++
              rslt1 ++
              [Cmp src1 (makeConstant IntType 0),
              CondJump CondE endStr,
              Copy dst (makeConstant IntType 0),
              Label endStr], PlainOperand dst)
    (TypedAST.Unary op expr' type_) -> do
      (rslt, src) <- exprToTACConvert name expr'
      dst <- makeTemp name type_
      return (rslt ++ [Unary op dst src], PlainOperand dst)
    (TypedAST.Var v _) -> return ([], PlainOperand $ Var v)
    (TypedAST.FunctionCall funcName args type_) -> do
      (rslts, srcs) <- argsToTAC name args
      dst <- makeTemp name type_
      return (rslts ++ [Call funcName dst srcs], PlainOperand dst)
    (TypedAST.Cast type_ expr') -> do
      (rslt, src) <- exprToTACConvert name expr'
      (cast, dst) <-
        if type_ == getExprType expr' then
          return ([], src) -- return rslt
        else do
          -- extend or truncate
          let oldType = getExprType expr'
          dst <- makeTemp name type_
          if typeSize type_ == typeSize oldType then
            return ([Copy dst src], dst)
          else error "long types not supported yet"
          --else if typeSize type_ < typeSize oldType then
          --  return [Truncate dst src]
          --else if isSigned type_ then
          --  return [SignExtend dst src]
          --else
          --  return [ZeroExtend dst src]
      return (rslt ++ cast, PlainOperand dst)
    (TypedAST.Dereference inner _) -> do
      (instrs, rslt) <- exprToTACConvert name inner
      return (instrs, DereferencedPointer rslt)
    (TypedAST.AddrOf inner type_) -> do
      (instrs, rslt) <- exprToTAC name inner
      case rslt of
        PlainOperand obj -> do
          dst <- makeTemp name type_
          return (instrs ++ [GetAddress dst obj], PlainOperand dst)
        DereferencedPointer ptr -> return (instrs, PlainOperand ptr)
    (TypedAST.Subscript left right type_) ->
      -- TODO: maybe optimize this
      if isPointerType (getExprType left) && isArithmeticType (getExprType right) then do
        (rslt1, src1) <- exprToTACConvert name left
        --dst1 <- makeTemp name
        (rslt2, src2) <- exprToTACConvert name right
        dst2 <- makeTemp name type_
        dst3 <- makeTemp name type_
        -- possible optimization: return dst1
        let refType = getRefType $ getExprType left
        let rightType = getExprType right
        return (rslt1 ++ rslt2 ++
          [ Binary MulOp dst2 src2 (makeConstant rightType $ typeSize refType) rightType,
            Binary AddOp dst3 src1 dst2 type_], DereferencedPointer dst3)
      else return $ error "Compiler Error: invalid add made it past typechecking"
