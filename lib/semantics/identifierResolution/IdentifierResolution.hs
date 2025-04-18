module IdentifierResolution where

import Utils
import SemanticsUtils

import AST

data MapEntry = MapEntry {
  entryName :: String,
  fromCurrentScope :: Bool,
  hasLinkage :: Bool
} deriving  (Show)

type IdentMap = [(String, MapEntry)]
type MapState a = StateT (IdentMap, Int) Result a

resolveFileScopeDclr :: Declaration -> MapState Declaration
resolveFileScopeDclr dclr = case dclr of
  VarDclr v -> do
    rslt <- resolveFileScopeVarDclr v
    return (VarDclr rslt)
  FunDclr f -> do
    rslt <- resolveFileScopeFunc f
    return (FunDclr rslt)

resolveFileScopeFunc :: FunctionDclr -> MapState FunctionDclr
resolveFileScopeFunc (FunctionDclr name type_ mStorage params body) = do
  maps <- getFst
  resolvedFunc <- case lookup name maps of
    (Just (MapEntry _ True False)) -> -- from current scope, no linkage
      return (Err $ "Semantics Error: Multiple declarations for " ++ name)
    (Just (MapEntry newName _ _)) -> do
      let newMaps = replace name (MapEntry newName True True) maps
      putFst newMaps
      newParams <- resolveParams params
      newBody <- liftMaybe resolveBlock body
      return (pure $ FunctionDclr newName type_ mStorage newParams newBody)
    Nothing -> do
      let newMaps = (name, MapEntry name True True) : maps
      let newerMaps = copyVarMap newMaps
      putFst newerMaps
      newParams <- resolveParams params
      newBody <- liftMaybe resolveBlock body
      putFst newMaps
      return (pure $ FunctionDclr name type_ mStorage newParams newBody)
  lift resolvedFunc

resolveLocalFunc :: FunctionDclr -> MapState FunctionDclr
resolveLocalFunc (FunctionDclr name type_ mStorage params body) = do
  maps <- getFst
  case mStorage of
    Just Static -> lift (Err $ "Semantics Error: cannot declare local function " ++ show name ++ " static")
    _ -> case lookup name maps of
      (Just (MapEntry _ True False)) -> -- from current scope, no linkage
        lift (Err $ "Semantics Error: Multiple declarations for variable " ++ show name)
      (Just (MapEntry {})) -> do
        case body of
          Just _ ->
            lift (Err $ "Semantics Error: Local function definition for " ++ show name)
          Nothing -> do
            putFst $ replace name (MapEntry name True True) maps
            return (FunctionDclr name type_ mStorage params body)
      Nothing -> do
        putFst $ (name, MapEntry name True True) : maps
        case body of
          Just _ ->
            lift (Err $ "Semantics Error: Local function definition for " ++ show name)
          Nothing -> return (FunctionDclr name type_ mStorage params body)

resolveParams :: [VariableDclr] -> MapState [VariableDclr]
resolveParams = foldr f (return [])
  where f x xs = do
          rslt <- resolveLocalVarDclr x
          rslts <- xs
          return (rslt : rslts)

resolveBlock :: Block -> MapState Block
resolveBlock (Block items) = do
  rslt <- foldl' resolveBlockItems (return []) items
  return (Block rslt)

resolveBlockItems :: MapState [BlockItem] -> BlockItem ->
    MapState [BlockItem]
resolveBlockItems oldState instr = do
  eInstrs <- oldState
  case instr of
    StmtBlock x -> do
      rslt <- resolveStmt x
      return (append eInstrs (StmtBlock rslt))
    DclrBlock x -> do
      resolved <- resolveLocalDclr x
      return (append eInstrs (DclrBlock resolved))

resolveStmt :: Stmt -> MapState Stmt
resolveStmt stmt = case stmt of
  RetStmt expr func -> do
    rslt <- resolveExpr expr
    return (RetStmt rslt func)
  ExprStmt expr -> do
    rslt <- resolveExpr expr
    return (ExprStmt rslt)
  IfStmt condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveStmt left
    rsltRight <- liftMaybe resolveStmt right
    return (IfStmt rsltCondition rsltLeft rsltRight)
  LabeledStmt label stmt' -> do
    rslt <- resolveStmt stmt'
    return (LabeledStmt label rslt)
  GoToStmt label -> return (GoToStmt label)
  CompoundStmt block -> do
    maps <- getFst
    let newMaps = copyVarMap maps
    putFst newMaps
    rslt <- resolveBlock block
    putFst maps
    return (CompoundStmt rslt)
  BreakStmt label -> return (BreakStmt label)
  ContinueStmt label -> return (ContinueStmt label)
  WhileStmt condition body label -> do
    rsltCondition <- resolveExpr condition
    rsltBody <- resolveStmt body
    return (WhileStmt rsltCondition rsltBody label)
  DoWhileStmt body condition label -> do
    rsltBody <- resolveStmt body
    rsltCondition <- resolveExpr condition
    return (DoWhileStmt rsltBody rsltCondition label)
  ForStmt init_ condition end body label -> do
    maps <- getFst
    let newMaps = copyVarMap maps
    putFst newMaps
    rsltInit <- resolveInit init_
    rsltCondition <- liftMaybe resolveExpr condition
    rsltEnd <- liftMaybe resolveExpr end
    rsltBody <- resolveStmt body
    putFst maps
    return (ForStmt rsltInit rsltCondition rsltEnd rsltBody label)
  SwitchStmt expr block label cases -> do
    rsltExpr <- resolveExpr expr
    rsltStmt <- resolveStmt block
    return (SwitchStmt rsltExpr rsltStmt label cases)
  CaseStmt expr stmt' label -> do
    n <- case evalConst expr of
      Just m -> return m
      Nothing -> lift (Err "Semantics Error: Case has non-constant value")
    liftA3 CaseStmt (pure $ Lit (ConstInt n)) (resolveStmt stmt') (pure label)
  DefaultStmt stmt' label -> liftA2 DefaultStmt (resolveStmt stmt') (pure label)
  NullStmt -> return NullStmt

resolveInit :: ForInit -> MapState ForInit
resolveInit init_ = case init_ of
  InitDclr d -> do
    rslt <- resolveLocalVarDclr d
    return (InitDclr rslt)
  InitExpr e -> do
    rslt <- liftMaybe resolveExpr e
    return (InitExpr rslt)

resolveLocalDclr :: Declaration -> MapState Declaration
resolveLocalDclr dclr = case dclr of
  VarDclr v -> do
    rslt <- resolveLocalVarDclr v
    return (VarDclr rslt)
  FunDclr f -> do
    rslt <- resolveLocalFunc f
    return (FunDclr rslt)

resolveLocalVarDclr :: VariableDclr -> MapState VariableDclr
resolveLocalVarDclr (VariableDclr name type_ mStorage mInit) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True True)) ->
      case mStorage of
        Just Extern -> return (VariableDclr name type_ mStorage mInit)
          -- previous declaration has linkage and so does this one; we're good
        _ -> lift (Err $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry _ True False)) ->
      lift (Err $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry _ False _)) ->
      case mStorage of
        Just Extern -> do
          let newMaps = replace name (MapEntry name True True) maps
          putFst newMaps
          return (VariableDclr name type_ mStorage mInit) -- no need to resolve mExpr, extern => it's a constant 
        _ -> do
          newName <- makeUnique name
          let newMaps = replace name (MapEntry newName True False) maps
          putFst newMaps
          newInit <- liftMaybe resolveVarInit mInit
          return (VariableDclr newName type_ mStorage newInit)
    Nothing ->
      case mStorage of
        Just Extern -> do
          let newMaps = (name, MapEntry name True False) : maps
          putFst newMaps
          return (VariableDclr name type_ mStorage mInit) -- no need to resolve mExpr, extern => it's a constant 
        _ -> do
          newName <- makeUnique name
          let newMaps = (name, MapEntry newName True False) : maps
          putFst newMaps
          newInit <- liftMaybe resolveVarInit mInit
          return (VariableDclr newName type_ mStorage newInit)

resolveVarInit :: VarInit -> MapState VarInit
resolveVarInit varInit = case varInit of
  SingleInit e -> SingleInit <$> resolveExpr e
  CompoundInit inits -> CompoundInit <$> traverse resolveVarInit inits

resolveFileScopeVarDclr :: VariableDclr -> MapState VariableDclr
resolveFileScopeVarDclr (VariableDclr name type_ mStorage mExpr) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True _)) ->
      return (VariableDclr name type_ mStorage mExpr)
    (Just (MapEntry _ False _)) -> error "Compiler Error: Function is outside file scope?"
    Nothing -> do
      let newMaps = (name, MapEntry name True True) : maps
      putFst newMaps
      -- expr should be a constant
      -- no need to recursively process
      return (VariableDclr name type_ mStorage mExpr)

resolveExpr :: Expr -> MapState Expr
resolveExpr expr = case expr of
  Assign left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Assign rsltLeft rsltRight)
  PostAssign expr' op -> do
    rsltExpr <- resolveExpr expr'
    return (PostAssign rsltExpr op)
  Binary PlusEqOp left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Binary PlusEqOp rsltLeft rsltRight)
  Binary MinusEqOp left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Binary MinusEqOp rsltLeft rsltRight)
  Binary op left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Binary op rsltLeft rsltRight)
  Conditional condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Conditional rsltCondition rsltLeft rsltRight)
  Lit n -> return (Lit n)
  Unary op expr' -> Unary op <$> resolveExpr expr'
  Var name -> do
    maps <- getFst
    case lookup name maps of
      Just (MapEntry newName _ _) -> return (Var newName)
      Nothing -> lift (Err $ "Semantics Error: No declaration for variable " ++ show name)
  FunctionCall name args -> do
    maps <- getFst
    case lookup name maps of
      Just entry -> do
        newArgs <- resolveArgs args
        return (FunctionCall (entryName entry) newArgs)
      Nothing -> lift (Err $ "Semantics Error: Function " ++ show name ++ " has not been declared")
  Cast target expr' -> Cast target <$> resolveExpr expr'
  AddrOf expr' -> AddrOf <$> resolveExpr expr'
  Dereference expr' -> Dereference <$> resolveExpr expr'
  Subscript left right -> Subscript <$> resolveExpr left <*> resolveExpr right

resolveArgs :: [Expr] -> MapState [Expr]
resolveArgs = foldr f (return [])
  where f x xs = do
          expr <- resolveExpr x
          exprs <- xs
          return (expr : exprs)

copyVarMap :: IdentMap -> IdentMap
copyVarMap = foldr (\(s, MapEntry s' _ l) maps ->
  (s, MapEntry s' False l) : maps) []