
module Typechecking where

import Utils
import SemanticsUtils
import qualified AST
import AST(BinOp(..), UnaryOp(..))
import TypedAST

typecheck :: AST.Prog -> StateT SymbolTable Result (SymbolTable, Prog)
typecheck (AST.Prog p) = do
  typedProg <- foldl' typecheckFold (return []) p
  symbols <- get
  return (symbols, Prog typedProg)

typecheckFold :: StateT SymbolTable Result [Declaration] ->  AST.Declaration ->
  StateT SymbolTable Result [Declaration]
typecheckFold oldState dclr = do
  -- set maps to whatever they were in the previous state
  typedDclrs <- oldState
  -- typecheck next item (as long as previous check succeeded)
  typedDclr <- typecheckFileScopeDclr dclr
  return (append typedDclrs typedDclr)

typecheckFileScopeDclr :: AST.Declaration -> StateT SymbolTable Result Declaration
typecheckFileScopeDclr dclr = case dclr of
  AST.VarDclr v -> VarDclr <$> typecheckFileScopeVar v
  AST.FunDclr f -> FunDclr <$> typecheckFunc f

hasInit :: IdentInit -> Bool
hasInit (Initial _) = True
hasInit _ = False

getGlobalVarAttrs :: IdentAttrs -> (IdentInit, Bool)
getGlobalVarAttrs attrs = case attrs of
  FunAttr _ _ -> error "Compiler Error: tried to typecheck function as variable?"
  StaticAttr init_ glb -> (init_, glb)
  LocalAttr -> error "Compiler Error: tried to typecheck local var as global?"

getFunAttrs :: IdentAttrs -> (Bool, Bool)
getFunAttrs attrs = case attrs of
  FunAttr def glb -> (def, glb)
  StaticAttr _ _ -> error
    "Compiler Error: Function shouldn't have static storage duration"
  LocalAttr -> (False, False)

typecheckFileScopeVar :: AST.VariableDclr -> StateT SymbolTable Result VariableDclr
typecheckFileScopeVar (AST.VariableDclr v type_ mStorage mExpr) = do
  (init_, expr) <- case isConst <$> mExpr of
    Just (Just i) -> return (intStaticInit type_ i, Just $ litExpr i type_) -- initializer was a constant
    Just Nothing -> lift (Err $ "Semantics Error: non-constant initializer for global variable " ++ show v)
    Nothing -> case mStorage of -- no initializer was provided
      Just Extern -> return (NoInit, Nothing)
      _ -> return (Tentative, Just $ litExpr 0 type_)
  let global = mStorage /= Just Static
  maps <- get
  (newInit, newGlobal) <-
    case lookup v maps of
      Nothing -> return (init_, global) -- no previous declaration for this variable
      Just oldDclr -> case oldDclr of
        (FunType _ _, _) -> lift (Err $ "Function " ++ show v ++ " redeclared as variable")
        (_, attrs) -> do
          let (oldInit, oldGlobal) = getGlobalVarAttrs attrs
          globalResult <- case mStorage of
            Just Extern -> pure oldGlobal -- extern => match storage of previous declaration
            _ ->
              if oldGlobal /= global
                then lift (Err $ "Conflicting variable linkage for variable " ++ show v)
              else return global
          initResult <-
            if hasInit oldInit
              then if hasInit init_
                then lift (Err $ "Conflicting file scope variable definitions for variable " ++ show v)
              else return oldInit -- can only initialize global vars once
            else if not (hasInit init_) && oldInit == Tentative
              then return Tentative
            else return init_
          return (initResult, globalResult)
  let attrs = StaticAttr newInit newGlobal
  put $ replace v (type_, attrs) maps
  return (VariableDclr v type_ mStorage expr)

typecheckFunc :: AST.FunctionDclr -> StateT SymbolTable Result FunctionDclr
typecheckFunc (AST.FunctionDclr name type_  mStorage params mBody) = do
  let hasBody = case mBody of
        Just _ -> True
        Nothing -> False
      global = mStorage /= Just Static
  maps <- get
  case lookup name maps of
    Just (oldType, oldAttrs) -> do
      let (alreadyDefined, oldGlobal) = getFunAttrs oldAttrs
      if oldType /= type_
        then lift (Err $ "Semantics Error: Incompatible function declarations for " ++ show name)
      else if oldGlobal && mStorage == Just Static
        then lift (Err $ "Semantics Error: Static function declaration follows non-static for function " ++ show name)
      else if hasBody && alreadyDefined
        then lift (Err $ "Semantics Error: Multiple definitions for function " ++ show name)
      else do
        let attrs = FunAttr (hasBody || alreadyDefined)  global
        put $ replace name (type_, attrs) maps
    Nothing -> do
      let attrs = FunAttr hasBody global
      put $ (name, (type_, attrs)) : maps

  typedParams <- typecheckParams params
  typedBody <- liftMaybe typecheckBlock mBody
  return (FunctionDclr name type_ mStorage typedParams typedBody)

typecheckParams :: [AST.VariableDclr] -> StateT SymbolTable Result [VariableDclr]
typecheckParams params = do
  maps <- get
  put $ maps ++ paramMaps params
  foldr typecheckParamsFold (pure []) params

typecheckParamsFold :: AST.VariableDclr ->
    StateT SymbolTable Result [VariableDclr] ->
    StateT SymbolTable Result [VariableDclr]
typecheckParamsFold (AST.VariableDclr name type_ Nothing Nothing) paramsState = do
  params <- paramsState
  return (VariableDclr name type_ Nothing Nothing : params)
typecheckParamsFold _ _ = error "Compiler Error: function parameter should not have initializer"

paramMaps :: [AST.VariableDclr] -> SymbolTable
paramMaps = foldr paramMapsFold []

paramMapsFold :: AST.VariableDclr -> SymbolTable -> SymbolTable
paramMapsFold (AST.VariableDclr name type_ Nothing Nothing) table =
  (name, (type_, LocalAttr)) : table
paramMapsFold _ _ = error "Compiler Error: function parameter should not have initializer"

typecheckBlock :: AST.Block -> StateT SymbolTable Result Block
typecheckBlock (AST.Block items) = Block <$> foldl' typecheckBlockFold (return []) items

typecheckBlockFold :: StateT SymbolTable Result [BlockItem] -> AST.BlockItem ->
  StateT SymbolTable Result [BlockItem]
typecheckBlockFold oldState item = do
  typedBlocks <- oldState
  typedBlock <- typecheckBlockItem item
  return (append typedBlocks typedBlock)

typecheckBlockItem :: AST.BlockItem -> StateT SymbolTable Result BlockItem
typecheckBlockItem item = case item of
  AST.StmtBlock stmt -> StmtBlock <$> typecheckStmt stmt
  AST.DclrBlock dclr -> DclrBlock <$> typecheckLocalDclr dclr

typecheckStmt :: AST.Stmt -> StateT SymbolTable Result Stmt
typecheckStmt stmt = case stmt of
  AST.RetStmt expr mFunc -> do
    typedExpr <- typecheckExpr expr
    maps <- get
    let retType = case lookup (fromJust mFunc) maps of
            (Just (FunType _ type_, _)) -> type_
            _ -> error "Compiler Error: this should be a function declaration"
    return (RetStmt (convertExprType typedExpr retType))
  AST.ExprStmt expr -> ExprStmt <$> typecheckExpr expr
  AST.IfStmt expr stmt1 mStmt2 -> do
    typedStmt2 <- liftMaybe typecheckStmt mStmt2
    typedExpr <- typecheckExpr expr
    typedStmt1 <- typecheckStmt stmt1
    return (IfStmt typedExpr typedStmt1 typedStmt2)
  AST.GoToStmt label -> return (GoToStmt label)
  AST.LabeledStmt name stmt' -> LabeledStmt name <$> typecheckStmt stmt'
  AST.CompoundStmt block -> CompoundStmt <$> typecheckBlock block
  AST.BreakStmt label -> return (BreakStmt label)
  AST.ContinueStmt label -> return (ContinueStmt label)
  AST.WhileStmt expr stmt' label -> do
    typedExpr <- typecheckExpr expr
    typedStmt <- typecheckStmt stmt'
    return (WhileStmt typedExpr typedStmt label)
  AST.DoWhileStmt stmt' expr label -> do
    typedStmt <- typecheckStmt stmt'
    typedExpr <- typecheckExpr expr
    return (DoWhileStmt typedStmt typedExpr label)
  AST.ForStmt init_ mExpr1 mExpr2 stmt' label -> do
    typedInit <- typecheckForInit init_
    typedExpr1 <- liftMaybe typecheckExpr mExpr1
    typedExpr2 <- liftMaybe typecheckExpr mExpr2
    typedStmt <- typecheckStmt stmt'
    return (ForStmt typedInit typedExpr1 typedExpr2 typedStmt label)
  AST.SwitchStmt expr stmt' label cases -> do
    typedExpr <- typecheckExpr expr
    typedStmt <- typecheckStmt stmt'
    return (SwitchStmt typedExpr typedStmt label cases)
  AST.CaseStmt expr stmt' label -> do
    typedStmt <- typecheckStmt stmt'
    typedExpr <- typecheckExpr expr
    return (CaseStmt typedExpr typedStmt label)
  AST.DefaultStmt stmt' label -> do
    typedStmt <- typecheckStmt stmt'
    return (DefaultStmt typedStmt label)
  AST.NullStmt -> return NullStmt

typecheckForInit :: AST.ForInit -> StateT SymbolTable Result ForInit
typecheckForInit init_ = case init_ of
  AST.InitDclr dclr -> InitDclr <$> typecheckLocalVar dclr
  AST.InitExpr (Just expr) -> InitExpr . Just <$> typecheckExpr expr
  AST.InitExpr Nothing -> return (InitExpr Nothing)

typecheckLocalDclr :: AST.Declaration -> StateT SymbolTable Result Declaration
typecheckLocalDclr dclr = case dclr of
  AST.VarDclr v -> VarDclr <$> typecheckLocalVar v
  AST.FunDclr f -> FunDclr <$> typecheckFunc f

getName :: String -> String
getName = show . takeWhile (/= '.')

typecheckLocalVar :: AST.VariableDclr -> StateT SymbolTable Result VariableDclr
typecheckLocalVar (AST.VariableDclr v type_ mStorage mExpr) = do
  maps <- get
  if mStorage == Just Extern then
    if isJust mExpr then
      lift (Err $ "Initializer on local extern variable declaration for variable " ++ getName v)
    else case lookup v maps of
      Nothing -> do
        put $ (v, (type_, StaticAttr NoInit True)) : maps -- add v to symbol table
        return (VariableDclr v type_ mStorage Nothing) -- mExpr was Nothing, no need to typecheck
      Just (FunType _ _, _) -> lift (Err $ "Function " ++ v ++ " redeclared as variable")
      Just (oldType, _) ->
        if type_ == oldType then
          return (VariableDclr v type_ mStorage Nothing) -- mExpr was Nothing, no need to typecheck
        else
          lift (Err $ "Conflicting types for variable " ++ getName v)
  else if mStorage == Just Static then
    case isConst <$> mExpr of
      Just (Just i) -> do -- the expr was constant, so we're good
        put $ (v, (type_, StaticAttr (intStaticInit type_ i) False)) : maps
        return (VariableDclr v type_ mStorage (Just $ litExpr i type_))
      Just Nothing -> lift (Err $ "Non-constant initializer on local static variable " ++ getName v)
      Nothing -> do -- there was no expr initializer
        put $ (v, (type_, StaticAttr (intStaticInit type_ 0) True)) : maps
        return (VariableDclr v type_ mStorage Nothing)
  else do -- it's a local variable, and we need to typecheck the initializer
    put ((v, (type_, LocalAttr)) : maps)
    typedExpr <- liftMaybe typecheckExpr mExpr
    let convertedRight = (`convertExprType` type_) <$> typedExpr
    return (VariableDclr v type_ mStorage convertedRight)

typecheckExpr :: AST.Expr -> StateT SymbolTable Result Expr
typecheckExpr e = case e of
  AST.Binary op left right -> do
    typedLeft <- typecheckExpr left
    typedRight <- typecheckExpr right
    if op == BoolAnd || op == BoolOr
      then return (Binary op typedLeft typedRight AST.IntType)
    else do
      let leftType = getExprType typedLeft
          rightType = getExprType typedRight
          commonType = getCommonType leftType rightType
          convertedLeft = convertExprType typedLeft commonType
          convertedRight = convertExprType typedRight commonType
      return (Binary op convertedLeft convertedRight $
        if op `elem` AST.relationalOps then
          IntType
        else
          commonType)
  AST.Assign left right -> do
    typedLeft <- typecheckExpr left
    typedRight <- typecheckExpr right
    let leftType = getExprType typedLeft
        convertedRight = convertExprType typedRight leftType
    return (Assign typedLeft convertedRight leftType)
  AST.PostAssign expr op -> do
    typedExpr <- typecheckExpr expr
    return (PostAssign typedExpr op (getExprType typedExpr))
  AST.Conditional c left right -> do
    typedC <- typecheckExpr c
    typedLeft <- typecheckExpr left
    typedRight <- typecheckExpr right
    let leftType = getExprType typedLeft
        rightType = getExprType typedRight
        commonType = getCommonType leftType rightType
        convertedLeft = convertExprType typedLeft commonType
        convertedRight = convertExprType typedRight commonType
    return (Conditional typedC convertedLeft convertedRight commonType)
  AST.FunctionCall name args -> do
    maps <- get
    case lookup name maps of
      Just (rsltType, _) -> case rsltType of
        AST.FunType paramTypes retType ->
          if length paramTypes /= length args then
            lift (Err $ "Function " ++ show name ++ " called with wrong number of arguments")
          else do
            argsRslt <- typecheckArgs args paramTypes
            return (FunctionCall name argsRslt retType)
        _ -> lift
          (Err $ "Variable " ++ show (head $ splitOn "." name) ++ " cannot be used as a function")
      Nothing -> error $ "Compiler Error: missed function declaration for " ++ show name
  AST.Var v -> do
    maps <- get
    case lookup v maps of
      Just (rsltType, _) -> case rsltType of
        AST.FunType _ _ -> lift (Err $ "Function " ++ show v ++ " cannot be used as a variable")
        type_ -> return (Var v type_)
      Nothing -> error $ "Compiler Error: missed variable declaration for " ++ show v
  AST.Unary op expr' -> do
    rslt <- typecheckExpr expr'
    let type_ = if op == BoolNot
        then IntType
        else getExprType rslt
    return (Unary op rslt type_)
  AST.Lit c -> case c of
    AST.ConstInt _ -> return (Lit c IntType)
    AST.ConstUInt _ -> return (Lit c UIntType)
  AST.Cast target expr -> do
    rslt <- typecheckExpr expr
    return (Cast target rslt)

convertExprType :: Expr -> Type_ -> Expr
convertExprType expr type_ =
  if getExprType expr == type_ then
    expr
  else Cast type_ expr

typecheckArgs :: [AST.Expr] -> [Type_] -> StateT SymbolTable Result [Expr]
typecheckArgs args types =
  foldr typecheckArgsFold (return []) (zip args types)

typecheckArgsFold :: (AST.Expr, Type_) ->
  StateT SymbolTable Result [Expr] ->
  StateT SymbolTable Result [Expr]
typecheckArgsFold (arg, paramType) args = do
  typedArg <- typecheckExpr arg
  typedArgs <- args
  return (convertExprType typedArg paramType: typedArgs)

getCommonType :: Type_ -> Type_ -> Type_
getCommonType t1 t2
  | t1 == t2 = t1
  | t1 == IntType = t2
  | otherwise = t1
