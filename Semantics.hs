{-# LANGUAGE FlexibleContexts #-}

module Semantics where

import Lexer
import Parser
import Control.Applicative
import Data.List ( foldl', deleteBy )
import Data.List.Split
import Control.Monad.State
import Control.Monad (when)
import Control.Applicative.HT (lift4, lift5)
import Data.Bits
import Data.Maybe

-- resolve variables, typecheck, label loops, and resolve labeled statements
-- for the entire program
resolveProg :: ASTProg -> Either String (SymbolTable, ASTProg)
resolveProg (ASTProg p) = do
  rslt <- ASTProg <$> resolveProgFold p
  symbols <- evalStateT (typecheck rslt) []
  return (symbols, rslt)

-- resolve each statements, while tracking the state of the variable map
resolveProgFold :: [Declaration] -> Either String [Declaration]
resolveProgFold fs = evalStateT (foldr f (pure []) fs) ([], 0)
  where f x xs = do
          dclr <- resolveFileScopeDclr x
          dclrs <- xs
          return (dclr : dclrs)

-------------- goto/labeled statement resolution ----------------

type LabelMap = [(String, String)]

createLabelMaps :: String -> FunctionDclr -> Either String LabelMap
createLabelMaps name (FunctionDclr _ _ _ (Just (Block items))) =
  foldr (createLabelMap name) (pure []) items
createLabelMaps name (FunctionDclr _ _ _ Nothing) = pure []

createLabelMap :: String -> BlockItem -> Either String LabelMap -> Either String LabelMap
createLabelMap name item eMaps =
  case item of
    DclrBlock _ -> eMaps -- can't label declarations, only statements
    StmtBlock stmt ->
      case stmt of
        -- if it's a labeled statement, add the label to the label map
        LabeledStmt label stmt -> do
          maps <- eMaps
          case lookup label maps of
            -- ensure there's no duplicate labels
            Just _ -> Left $ "Semantics Error: Multiple definitions for label " ++ show label
            Nothing -> do
              -- make a name unique to this function
              let newLabel = name ++ "." ++ label
              createLabelMap name (StmtBlock stmt) (pure $ (label, newLabel) : maps)

        -- recursively check for more labeled statements
        IfStmt _ stmt1 mStmt2 -> do
          newMaps <- createLabelMap name (StmtBlock stmt1) eMaps
          case mStmt2 of
            Just stmt2 -> createLabelMap name (StmtBlock stmt2) (pure newMaps)
            Nothing -> pure newMaps
        CompoundStmt (Block items) -> foldr (createLabelMap name) (pure []) items
        WhileStmt _ stmt _ -> createLabelMap name (StmtBlock stmt) eMaps
        DoWhileStmt stmt _ _ -> createLabelMap name (StmtBlock stmt) eMaps
        ForStmt _ _ _ stmt _ -> createLabelMap name (StmtBlock stmt) eMaps
        SwitchStmt _ stmt _ _ -> createLabelMap name (StmtBlock stmt) eMaps
        CaseStmt _ stmt _ ->  createLabelMap name (StmtBlock stmt) eMaps
        DefaultStmt stmt _ ->  createLabelMap name (StmtBlock stmt) eMaps
        _ -> eMaps -- other statements are not recursive

resolveLabels :: String -> LabelMap -> FunctionDclr -> Either String FunctionDclr
resolveLabels name maps (FunctionDclr fName mStorage params (Just (Block items))) =
  FunctionDclr fName mStorage params . Just <$> foldr (resolveBlockLabels name maps) (pure $ Block []) items
resolveLabels name maps f@(FunctionDclr _ _ _ Nothing) =
  pure f

resolveBlockLabels :: String -> LabelMap -> BlockItem -> Either String Block -> Either String Block
resolveBlockLabels name maps item eItems = do
  (Block items) <- eItems
  case resolveItemLabels name maps item of
    Right resolved -> return (Block $ resolved : items)
    Left s -> Left s

resolveItemLabels :: String -> LabelMap -> BlockItem -> Either String BlockItem
resolveItemLabels name maps item =
  case item of
    DclrBlock _ -> return item
    StmtBlock stmt -> StmtBlock <$> resolveLabel name maps stmt

resolveLabel :: String -> LabelMap -> ASTStmt -> Either String ASTStmt
resolveLabel name maps item =
    case item of
      -- ensure there's a label to jump to
      GoToStmt label ->
        case lookup label maps of
          Just newLabel -> return (GoToStmt newLabel)
          Nothing -> Left $ "Semantics Error: Label " ++ show label ++ " has no definition"

      -- recursively check statements for labels
      LabeledStmt label stmt ->
        case lookup label maps of
          Just newLabel -> LabeledStmt newLabel <$> resolveLabel name maps stmt
          Nothing -> error $
            "Compiler Error: No label was created for the labeled statement: " ++ show label
      IfStmt expr stmt1 mStmt2 -> do
        resolved1 <- resolveLabel name maps stmt1
        case mStmt2 of
          Just stmt2 -> do
            resolved2 <- resolveLabel name maps stmt2
            return (IfStmt expr resolved1 (Just resolved2))
          Nothing -> return (IfStmt expr resolved1 Nothing)
      CompoundStmt (Block items) ->
        CompoundStmt <$> foldr (resolveBlockLabels name maps) (pure $ Block []) items
      WhileStmt expr stmt label -> liftA3 WhileStmt (pure expr) (resolveLabel name maps stmt) (pure label)
      DoWhileStmt stmt expr label -> liftA3 DoWhileStmt (resolveLabel name maps stmt) (pure expr) (pure label)
      ForStmt init condition end stmt label ->
        lift5 ForStmt (pure init) (pure condition) (pure end) (resolveLabel name maps stmt) (pure label)
      SwitchStmt expr stmt label cases -> lift4 SwitchStmt (pure expr) (resolveLabel name maps stmt) (pure label) (pure cases)
      CaseStmt expr stmt label -> liftA3 CaseStmt (pure expr) (resolveLabel name maps stmt) (pure label)
      DefaultStmt stmt label -> liftA2 DefaultStmt (resolveLabel name maps stmt) (pure label)
      _ -> return item

-------------------- Loop Labeling -----------------------------------

-- (Maybe String, Maybe String, Bool), Int
-- Int is used for global counter
-- fst Maybe String stores most recent enclosing loop label
-- snd Maybe String stores most recent enclosing switch label
-- Bool tracks what was more recent: True -> loop, False -> switch

labelStmt :: String -> ASTStmt -> StateT ((Maybe String, Maybe String, Bool), Int) (Either String) ASTStmt
labelStmt name stmt = case stmt of
  BreakStmt _ -> do
    (mLoopLabel, mSwitchLabel, b) <- getFst
    if b
      then case mLoopLabel of
        Nothing -> lift (Left "Semantics Error: Break statement outside loop/switch")
        Just label -> return (BreakStmt mLoopLabel)
    else case mSwitchLabel of
        Nothing -> lift (Left "Semantics Error: Break statement outside loop/switch")
        Just label -> return (BreakStmt mSwitchLabel)
  ContinueStmt _ -> do
    (mLabel, _, _) <- getFst
    case mLabel of
      Nothing -> lift (Left "Semantics Error: Continue statement outside loop")
      Just label -> return (ContinueStmt mLabel)
  WhileStmt condition body _ -> do
    label <- makeUnique $ name ++ ".while"
    oldLabel@(_, mSwitchLabel, _) <- getFst
    putFst (Just label, mSwitchLabel, True)
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (WhileStmt condition labeledBody (pure label))
  DoWhileStmt body condition _ -> do
    label <- makeUnique $ name ++ ".doWhile"
    oldLabel@(_, mSwitchLabel, _) <- getFst
    putFst (Just label, mSwitchLabel, True)
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (DoWhileStmt labeledBody condition (pure label))
  ForStmt init condition end body _ -> do
    label <- makeUnique $ name ++ ".for"
    oldLabel@(_, mSwitchLabel, _) <- getFst
    putFst (Just label, mSwitchLabel, True)
    labeledBody <- labelStmt name body
    putFst oldLabel
    return (ForStmt init condition end labeledBody (pure label))
  IfStmt condition left right -> do
    labeledLeft <- labelStmt name left
    labeledRight <- liftMaybe (labelStmt name) right
    return (IfStmt condition labeledLeft labeledRight)
  CompoundStmt block -> do
    labeledBlock <- labelBlock name block
    return (CompoundStmt labeledBlock)
  LabeledStmt s stmt -> do
    labeledStmt <- labelStmt name stmt
    return (LabeledStmt s labeledStmt)
  CaseStmt expr stmt _ -> do
    (_, mLabel, _) <- getFst
    label <- case mLabel of
      Nothing -> lift (Left "Semantics Error: Case statement outside switch")
      Just l -> return l
    labeledStmt <- labelStmt name stmt
    n <- case expr of
      Factor (ASTLit n) -> return n
      _ -> error "Compiler Error: Case has none constant expr"
    return (CaseStmt expr labeledStmt (Just $ label ++ "." ++ show n))
  DefaultStmt stmt _ -> do
    (_, mLabel, _) <- getFst
    label <- case mLabel of
      Nothing -> lift (Left "Semantics Error: Default statement outside switch")
      Just l -> return l
    labeledStmt <- labelStmt name stmt
    return (DefaultStmt labeledStmt (Just $ label ++ ".default"))
  SwitchStmt expr stmt label cases -> do
    label <- makeUnique $ name ++ ".switch"
    oldLabel@(mLoopLabel, _, _) <- getFst
    putFst (mLoopLabel, Just label, False)
    labeledBody <- labelStmt name stmt
    putFst oldLabel
    return (SwitchStmt expr labeledBody (pure label) cases)
  stmt -> return stmt

labelBlockItem :: String -> BlockItem ->
    StateT ((Maybe String, Maybe String, Bool), Int) (Either String) BlockItem
labelBlockItem name item = case item of
  StmtBlock stmt -> do
    rslt <- labelStmt name stmt
    return (StmtBlock rslt)
  DclrBlock dclr -> return (DclrBlock dclr)

labelBlock :: String -> Block -> StateT ((Maybe String, Maybe String, Bool), Int) (Either String) Block
labelBlock name (Block items) =  do
    -- BlockItem ->
    --  StateT (Maybe String, Int) (Either String) [BlockItem] ->
    --  StateT (Maybe String, Int) (Either String) [BlockItem]
    labeledItems <- foldr (liftA2 (:) . labelBlockItem name)
      (return []) items
    return (Block labeledItems)

-------------------- Finding Cases in Switch statement -------------------------------------

collectCasesStmt :: ASTStmt -> StateT [CaseLabel] (Either String) ASTStmt
collectCasesStmt stmt = case stmt of
  WhileStmt condition body label -> do
    newBody <- collectCasesStmt body
    return (WhileStmt condition newBody label)
  DoWhileStmt body condition label -> do
    newBody <- collectCasesStmt body
    return (DoWhileStmt newBody condition label)
  ForStmt init condition end body label -> do
    newBody <- collectCasesStmt body
    return (ForStmt init condition end newBody label)
  IfStmt condition left right -> do
    newLeft <- collectCasesStmt left
    newRight <- liftMaybe collectCasesStmt right
    return (IfStmt condition newLeft newRight)
  CompoundStmt block -> do
    newBlock <- collectCaseBlock block
    return (CompoundStmt newBlock)
  LabeledStmt s stmt -> do
    newStmt <- collectCasesStmt stmt
    return (LabeledStmt s newStmt)
  CaseStmt expr stmt label -> do
    cases <- get
    case expr of
      (Factor (ASTLit n)) -> if IntCase n `elem` cases
        then lift (Left "Semantics Error: Duplicate cases")
        else put $ cases ++ [IntCase n]
      _ -> error "Compiler Error: Case expr was not converted to lit"
    newStmt <- collectCasesStmt stmt
    return (CaseStmt expr newStmt label)
  DefaultStmt stmt label -> do
    cases <- get
    if DefaultCase `elem` cases
      then lift (Left "Semantics Error: Duplicate default cases")
      else put $ cases ++ [DefaultCase]
    newStmt <- collectCasesStmt stmt
    return (DefaultStmt newStmt label)
  SwitchStmt expr stmt label cases -> do
    oldCases <- get
    put []
    newStmt <- collectCasesStmt stmt
    rslt <- get
    put oldCases
    return (SwitchStmt expr newStmt label (Just rslt))
  stmt -> return stmt

collectCaseBlockItem :: BlockItem -> StateT [CaseLabel] (Either String) BlockItem
collectCaseBlockItem item = case item of
  StmtBlock stmt -> do
    rslt <- collectCasesStmt stmt
    return (StmtBlock rslt)
  DclrBlock dclr -> return (DclrBlock dclr)

collectCaseBlock :: Block -> StateT [CaseLabel] (Either String) Block
collectCaseBlock (Block items) =  do
    -- BlockItem ->
    --  StateT (Maybe String, Int) (Either String) [BlockItem] ->
    --  StateT (Maybe String, Int) (Either String) [BlockItem]
    labeledItems <- foldr (liftA2 (:) . collectCaseBlockItem)
      (return []) items
    return (Block labeledItems)

-------------------- Type Checking -------------------------------------

data Type_ = IntType | FunType Int -- param count
  deriving (Show, Eq)
type SymbolTable = [(String, (Type_, IdentAttrs))]

data IdentAttrs = FunAttr Bool Bool -- FunAttr defined global
                | StaticAttr IdentInit Bool -- StaticAttr init global
                | LocalAttr
                deriving (Show)

data IdentInit = Tentative | Initial Int | NoInit
  deriving (Show, Eq)

typecheck :: ASTProg -> StateT SymbolTable (Either String) SymbolTable
typecheck (ASTProg p) = do
  foldl' typecheckFold (return ()) p
  get

typecheckFold :: StateT SymbolTable (Either String) () ->  Declaration ->
  StateT SymbolTable (Either String) ()
typecheckFold oldState dclr = do
  -- set maps to whatever they were in the previous state
  maps <- do
    oldState
    get
  put maps
  -- typecheck next item (as long as previous check succeeded)
  typecheckFileScopeDclr dclr

typecheckFileScopeDclr :: Declaration -> StateT SymbolTable (Either String) ()
typecheckFileScopeDclr dclr = case dclr of
  VarDclr v -> typecheckFileScopeVar v
  FunDclr f -> typecheckFunc f

hasInit :: IdentInit -> Bool
hasInit (Initial _) = True
hasInit _ = False

getGlobalVarAttrs :: IdentAttrs -> (IdentInit, Bool)
getGlobalVarAttrs attrs = case attrs of
  FunAttr _ _ -> error "Compiler Error: tried to typecheck function as variable?"
  StaticAttr init glb -> (init, glb)
  LocalAttr -> error "Compiler Error: tried to typecheck local var as global?"

getFunAttrs :: IdentAttrs -> (Bool, Bool)
getFunAttrs attrs = case attrs of
  FunAttr def glb -> (def, glb)
  StaticAttr _ _ -> error
    "Compiler Error: Function shoudln't have static storage duration"
  LocalAttr -> (False, False)

typecheckFileScopeVar :: VariableDclr -> StateT SymbolTable (Either String) ()
typecheckFileScopeVar (VariableDclr v mStorage mExpr) = do
  init <- case isConst <$> mExpr of
    Just (Just i) -> return (Initial i) -- initializer was a constant
    Just Nothing -> lift (Left $ "Semantics Error: non-constant initializer for global variable " ++ show v)
    Nothing -> case mStorage of -- no initializer was provided
      Just Extern -> return NoInit
      _ -> return Tentative
  let global = mStorage /= Just Static
  maps <- get
  (init, global) <-
    case lookup v maps of
      Nothing -> return (init, global) -- no previous declaration for this variable
      Just oldDclr -> case oldDclr of
        (IntType, attrs) -> do
          let (oldInit, oldGlobal) = getGlobalVarAttrs attrs
          global <- case mStorage of
            Just Extern -> pure oldGlobal -- extern => match storage of previous declaration
            _ ->
              if oldGlobal /= global
                then lift (Left $ "Conflicting variable linkage for variable " ++ show v)
              else return global
          init <-
            if hasInit oldInit
              then if hasInit init
                then lift (Left $ "Conflicting file scope variable definitions for variable" ++ show v)
              else return oldInit -- can only initialize global vars once
            else if not (hasInit init) && oldInit == Tentative
              then return Tentative
            else return init
          return (init, global)
        _ -> lift (Left $ "Function " ++ show v ++ " redeclared as variable")
  let attrs = StaticAttr init global
  put $ (v, (IntType, attrs)) : maps

typecheckFunc :: FunctionDclr -> StateT SymbolTable (Either String) ()
typecheckFunc (FunctionDclr name mStorage params mBody) = do
  let funType = FunType $ length params
      hasBody = case mBody of
        Just _ -> True
        Nothing -> False
      global = mStorage /= Just Static
  maps <- get
  case lookup name maps of
    Just (oldType, oldAttrs) -> do
      let (alreadyDefined, oldGlobal) = getFunAttrs oldAttrs
      if oldType /= funType
        then lift (Left $ "Semantics Error: Incompatible function declarations for " ++ show name)
      else if oldGlobal && mStorage == Just Static
        then lift (Left $ "Semantics Error: Static function declaration follows non-static for function " ++ show name)
      else if hasBody && alreadyDefined
        then lift (Left $ "Semantics Error: Multiple definitions for function " ++ show name)
      else do
        let attrs = FunAttr (hasBody || alreadyDefined)  global
        put $ replace name (funType, attrs) maps
    Nothing -> do
      let attrs = FunAttr hasBody global
      put $ (name, (funType, attrs)) : maps
  case mBody of
    Just body -> do
      typecheckParams params
      typecheckBlock body
    Nothing -> return ()

typecheckParams :: [VariableDclr] -> StateT SymbolTable (Either String) ()
typecheckParams params = do
  maps <- get
  put $ maps ++ paramMaps params
  return ()

paramMaps :: [VariableDclr] -> SymbolTable
paramMaps = foldr paramMapsFold []

paramMapsFold :: VariableDclr -> SymbolTable -> SymbolTable
paramMapsFold (VariableDclr name Nothing mStorage) table =
  (name, (IntType, LocalAttr)) : table
paramMapsFold _ _ = error "Compiler Error: function parameter should not have initializer"

typecheckBlock :: Block -> StateT SymbolTable (Either String) ()
typecheckBlock (Block items) = foldl' typecheckBlockFold (return ()) items

typecheckBlockFold :: StateT SymbolTable (Either String) () -> BlockItem ->
  StateT SymbolTable (Either String) ()
typecheckBlockFold oldState item = do
  oldState
  typecheckBlockItem item

typecheckBlockItem :: BlockItem -> StateT SymbolTable (Either String) ()
typecheckBlockItem item = case item of
  StmtBlock stmt -> typecheckStmt stmt
  DclrBlock dclr -> typecheckLocalDclr dclr

typecheckStmt :: ASTStmt -> StateT SymbolTable (Either String) ()
typecheckStmt stmt = case stmt of
  RetStmt expr -> typecheckExpr expr
  ExprStmt expr -> typecheckExpr expr
  IfStmt expr stmt1 mStmt2 -> do
    liftMaybe typecheckStmt mStmt2
    typecheckExpr expr
    typecheckStmt stmt1
  GoToStmt label -> return ()
  LabeledStmt name stmt -> typecheckStmt stmt
  CompoundStmt block -> typecheckBlock block
  BreakStmt _ -> return ()
  ContinueStmt _ -> return ()
  WhileStmt expr stmt _ -> do
    typecheckExpr expr
    typecheckStmt stmt
  DoWhileStmt stmt expr _ -> do
    typecheckStmt stmt
    typecheckExpr expr
  ForStmt init mExpr1 mExpr2 stmt _ -> do
    typecheckForInit init
    liftMaybe typecheckExpr mExpr1
    liftMaybe typecheckExpr mExpr2
    typecheckStmt stmt
  SwitchStmt expr stmt _ _ -> do
    typecheckExpr expr
    typecheckStmt stmt
  CaseStmt _ stmt _ -> typecheckStmt stmt
  DefaultStmt stmt _ -> typecheckStmt stmt
  NullStmt -> return ()

typecheckForInit :: ForInit -> StateT SymbolTable (Either String) ()
typecheckForInit init = case init of
  InitDclr dclr -> typecheckLocalVar dclr
  InitExpr (Just expr) -> typecheckExpr expr
  InitExpr Nothing -> return ()

typecheckLocalDclr :: Declaration -> StateT SymbolTable (Either String) ()
typecheckLocalDclr dclr = case dclr of
  VarDclr v -> typecheckLocalVar v
  FunDclr f -> typecheckFunc f

getName :: String -> String
getName = show . takeWhile (/= '.')

typecheckLocalVar :: VariableDclr -> StateT SymbolTable (Either String) ()
typecheckLocalVar (VariableDclr v mStorage mExpr) = do
  maps <- get
  if mStorage == Just Extern
    then if isJust mExpr
      then lift (Left $ "Initializer on local extern variable declaration for variable " ++ getName v)
    else case lookup v maps of
      Nothing -> put $ (v, (IntType, StaticAttr NoInit True)) : maps -- add v to symbol table
      Just (IntType, oldAttrs) -> return ()
      _ -> lift (Left $ "Function " ++ v ++ " redeclared as variable")
  else if mStorage == Just Static
    then
      case isConst <$> mExpr of
        Just (Just i) ->
          put $ (v, (IntType, StaticAttr (Initial i) True)) : maps
        Just Nothing -> lift (Left $ "Non-constant initializer on local static variable " ++ getName v)
        Nothing -> put $ (v, (IntType, StaticAttr (Initial 0) True)) : maps
  else do
    put ((v, (IntType, LocalAttr)) : maps)
    mapM_ typecheckExpr mExpr

typecheckExpr :: ASTExpr -> StateT SymbolTable (Either String) ()
typecheckExpr e = case e of
  Factor f -> typecheckFactor f
  ASTBinary _ left right -> do
    typecheckExpr left
    typecheckExpr right
  ASTAssign left right -> do
    typecheckExpr left
    typecheckExpr right
  ASTPostAssign left right -> do
    typecheckExpr left
    typecheckExpr right
  Conditional c left right -> do
    typecheckExpr c
    typecheckExpr left
    typecheckExpr right

typecheckFactor :: Factor -> StateT SymbolTable (Either String) ()
typecheckFactor f = case f of
  FunctionCall name args -> do
    maps <- get
    case lookup name maps of
      Just (rsltType, _) -> case rsltType of
        IntType -> lift
          (Left $ "Variable " ++ show (head $ splitOn "." name) ++ " cannot be used as a function")
        FunType n -> if n == length args
          then typecheckArgs args
          else lift (Left $ "Function " ++ show name ++ " called with wrong number of arguments")
      Nothing -> error $ "Compiler Error: missed function declaration for " ++ show f
  ASTVar v -> do
    maps <- get
    case lookup v maps of
      Just (rsltType, _) -> case rsltType of
        IntType -> return ()
        FunType _ -> lift (Left $ "Function " ++ show v ++ "cannot be used as a variable")
      Nothing -> error $ "Compiler Error: missed variable declaration for " ++ show v
  ASTUnary _ f -> typecheckFactor f
  FactorExpr expr -> typecheckExpr expr
  ASTLit _ -> return ()

typecheckArgs :: [ASTExpr] -> StateT SymbolTable (Either String) ()
typecheckArgs =
  foldr (\x rslt -> rslt >> typecheckExpr x) (return ())

-- evaluate const expression, return Nothing for non-const
isConst :: ASTExpr -> Maybe Int
isConst expr = case expr of
  Factor f -> isFactorConst f
  ASTBinary SubOp left right -> liftA2 (-) (isConst left) (isConst right)
  ASTBinary AddOp left right -> liftA2 (+) (isConst left) (isConst right)
  ASTBinary MulOp left right -> liftA2 (*) (isConst left) (isConst right)
  ASTBinary DivOp left right -> liftA2 div (isConst left) (isConst right)
  ASTBinary ModOp left right -> liftA2 mod (isConst left) (isConst right)
  ASTBinary BitAnd left right -> liftA2 (.&.) (isConst left) (isConst right)
  ASTBinary BitOr left right -> liftA2 (.|.) (isConst left) (isConst right)
  ASTBinary BitXor left right -> liftA2 xor (isConst left) (isConst right)
  ASTBinary BitShr left right -> liftA2 shift (isConst left) ((* (-1)) <$> isConst right)
  ASTBinary BitShl left right -> liftA2 shift (isConst left) (isConst right)
  ASTBinary BoolAnd left right -> boolToInt <$> liftA2 (&&) (exprToBool left) (exprToBool right)
  ASTBinary BoolOr left right -> boolToInt <$> liftA2 (||) (exprToBool left) (exprToBool right)
  ASTBinary BoolEq left right -> boolToInt <$> liftA2 (==) (exprToBool left) (exprToBool right)
  ASTBinary BoolLe left right -> boolToInt <$> liftA2 (<) (exprToBool left) (exprToBool right)
  ASTBinary BoolGe left right -> boolToInt <$> liftA2 (>) (exprToBool left) (exprToBool right)
  ASTBinary BoolLeq left right -> boolToInt <$> liftA2 (<=) (exprToBool left) (exprToBool right)
  ASTBinary BoolGeq left right -> boolToInt <$> liftA2 (>=) (exprToBool left) (exprToBool right)
  ASTBinary _ _ _ -> Nothing
  ASTAssign _ _ -> Nothing
  ASTPostAssign _ _ -> Nothing
  Conditional c trueExpr falseExpr ->
    isConst c >>= (\x -> if x /= 0 then isConst trueExpr else isConst falseExpr)

isFactorConst :: Factor -> Maybe Int
isFactorConst f = case f of
  ASTLit n -> pure n
  ASTUnary Complement f -> complement <$> isFactorConst f
  ASTUnary Negate f -> (* (- 1)) <$> isFactorConst f
  ASTUnary BoolNot f -> (\x -> if x == 0 then 1 else 0) <$> isFactorConst f
  FactorExpr expr -> isConst expr
  ASTVar _ -> Nothing
  FunctionCall _ _ -> Nothing

boolToInt :: Bool -> Int
boolToInt b = if b then 1 else 0

exprToBool :: ASTExpr -> Maybe Bool
exprToBool expr = isConst expr >>= (\x -> if x /= 0 then pure True else pure False)

showSymbols :: Either String SymbolTable -> String
showSymbols (Right symbols) =
  symbols >>= (\(ident, attrs) -> show ident ++ ": " ++ show attrs ++ "\n")
showSymbols (Left msg) = msg

-------------------- Identifier Resolution -----------------------------

data MapEntry = MapEntry {
  entryName :: String,
  fromCurrentScope :: Bool,
  hasLinkage :: Bool
}

type IdentMap = [(String, MapEntry)]
type MapState a = StateT (IdentMap, Int) (Either String) a

resolveFileScopeDclr :: Declaration -> MapState Declaration
resolveFileScopeDclr dclr = case dclr of
  VarDclr v -> do
    rslt <- resolveFileScopeVarDclr v
    return (VarDclr rslt)
  FunDclr f -> do
    rslt <- resolveFileScopeFunc f
    return (FunDclr rslt)

resolveFileScopeFunc :: FunctionDclr -> MapState FunctionDclr
resolveFileScopeFunc (FunctionDclr name storageClass params body) = do
  maps <- getFst
  resolvedFunc <- case lookup name maps of
    (Just (MapEntry _ True False)) ->
      return (Left $ "Semantics Error: Multiple declarations for " ++ name)
    (Just (MapEntry newName scope link)) -> do
      let newMaps = replace name (MapEntry newName True True) maps
      putFst newMaps
      newParams <- resolveParams params
      newBody <- liftMaybe resolveBlock body
      return (pure $ FunctionDclr newName storageClass newParams newBody)
    Nothing -> do
      let newMaps = (name, MapEntry name True True) : maps
      let newerMaps = copyVarMap newMaps
      putFst newerMaps
      newParams <- resolveParams params
      newBody <- liftMaybe resolveBlock body
      putFst newMaps
      return (pure $ FunctionDclr name storageClass newParams newBody)
  let labeledLoops = do
        (FunctionDclr newName mStorage newParams newBody) <- resolvedFunc
        FunctionDclr newName mStorage newParams <$>
          evalStateT (liftMaybe (labelBlock name) newBody) ((Nothing, Nothing, False), 0)
      resolvedLabels = do
        rslt <- labeledLoops
        -- labels are specific to each function, so they're resolved here
        maps <- createLabelMaps name rslt
        resolveLabels name maps rslt
      resolvedSwitches = do
        (FunctionDclr newName mStorage newParams newBody) <- resolvedLabels
        FunctionDclr newName mStorage newParams <$>
          evalStateT (liftMaybe collectCaseBlock newBody) []
  lift resolvedSwitches

resolveLocalFunc :: FunctionDclr -> MapState FunctionDclr
resolveLocalFunc (FunctionDclr name mStorage params body) = do
  maps <- getFst
  case mStorage of
    Just Static -> lift (Left $ "Semantics Error: cannot declare local function " ++ show name ++ " static")
    _ -> case lookup name maps of
      (Just (MapEntry _ True False)) ->
        lift (Left $ "Semantics Error: Multiple declarations for variable " ++ show name)
      (Just (MapEntry newName scope link)) -> do
        let newMaps = replace name (MapEntry newName True True) maps
        putFst newMaps
        case body of
          Just b ->
            lift (Left $ "Semantics Error: Local function definition for " ++ show name)
          Nothing -> return (FunctionDclr newName mStorage params body)
      Nothing -> do
        putFst $ (name, MapEntry name True True) : maps
        case body of
          Just b ->
            lift (Left $ "Semantics Error: Local function definition for " ++ show name)
          Nothing -> return (FunctionDclr name mStorage params body)

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

resolveStmt :: ASTStmt -> MapState ASTStmt
resolveStmt stmt = case stmt of
  RetStmt expr -> do
    rslt <- resolveExpr expr
    return (RetStmt rslt)
  ExprStmt expr -> do
    rslt <- resolveExpr expr
    return (ExprStmt rslt)
  IfStmt condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveStmt left
    rsltRight <- liftMaybe resolveStmt right
    return (IfStmt rsltCondition rsltLeft rsltRight)
  LabeledStmt label stmt -> do
    rslt <- resolveStmt stmt
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
  ForStmt init condition end body label -> do
    maps <- getFst
    let newMaps = copyVarMap maps
    putFst newMaps
    rsltInit <- resolveInit init
    rsltCondition <- liftMaybe resolveExpr condition
    rsltEnd <- liftMaybe resolveExpr end
    rsltBody <- resolveStmt body
    putFst maps
    return (ForStmt rsltInit rsltCondition rsltEnd rsltBody label)
  SwitchStmt expr block label cases -> do
    rsltExpr <- resolveExpr expr
    rsltStmt <- resolveStmt block
    return (SwitchStmt rsltExpr rsltStmt label cases)
  CaseStmt expr stmt label -> do
    n <- case isConst expr of
      Just m -> return m
      Nothing -> lift (Left "Semantics Error: Case has non-constant value")
    liftA3 CaseStmt (pure . Factor $ ASTLit n) (resolveStmt stmt) (pure label)
  DefaultStmt stmt label -> liftA2 DefaultStmt (resolveStmt stmt) (pure label)
  NullStmt -> return NullStmt

resolveInit :: ForInit -> MapState ForInit
resolveInit init = case init of
  InitDclr d -> do
    rslt <- resolveLocalVarDclr d
    return (InitDclr rslt)
  InitExpr e -> do
    rslt <- liftMaybe resolveExpr e
    return (InitExpr rslt)

replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace x y maps =
  case lookup x maps of
    Just _ -> (x, y) : deleteBy (\(a, _) (a', _) -> a == a') (x, y) maps
    Nothing -> (x, y) : maps

copyVarMap :: IdentMap -> IdentMap
copyVarMap = foldr (\(s, MapEntry s' _ l) maps ->
  (s, MapEntry s' False l) : maps) []

resolveLocalDclr :: Declaration -> MapState Declaration
resolveLocalDclr dclr = case dclr of
  VarDclr v -> do
    rslt <- resolveLocalVarDclr v
    return (VarDclr rslt)
  FunDclr f -> do
    rslt <- resolveLocalFunc f
    return (FunDclr rslt)

resolveLocalVarDclr :: VariableDclr -> MapState VariableDclr
resolveLocalVarDclr (VariableDclr name mStorage mExpr) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True True)) ->
      case mStorage of
        Just Extern -> return (VariableDclr name mStorage mExpr)
          -- previous declaration has linkage and so does this one; we're good
        _ -> lift (Left $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry _ True False)) ->
      lift (Left $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry _ False link)) ->
      case mStorage of
        Just Extern -> do
          let newMaps = replace name (MapEntry name True True) maps
          putFst newMaps
          return (VariableDclr name mStorage mExpr) -- no need to resolve mExpr, extern => it's a constant 
        _ -> do
          newName <- makeUnique name
          let newMaps = replace name (MapEntry newName True False) maps
          putFst newMaps
          newExpr <- liftMaybe resolveExpr mExpr
          return (VariableDclr newName mStorage newExpr)
    Nothing ->
      case mStorage of
        Just Extern -> do
          let newMaps = (name, MapEntry name True False) : maps
          putFst newMaps
          return (VariableDclr name mStorage mExpr) -- no need to resolve mExpr, extern => it's a constant 
        _ -> do
          newName <- makeUnique name
          let newMaps = (name, MapEntry newName True False) : maps
          putFst newMaps
          newExpr <- liftMaybe resolveExpr mExpr
          return (VariableDclr newName mStorage newExpr)

resolveFileScopeVarDclr :: VariableDclr -> MapState VariableDclr
resolveFileScopeVarDclr (VariableDclr name mStorage mExpr) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True _)) ->
      return (VariableDclr name mStorage mExpr)
    (Just (MapEntry _ False _)) -> error "Compiler Error: Function is outside file scope?"
    Nothing -> do
      let newMaps = (name, MapEntry name True True) : maps
      putFst newMaps
      -- expr should be a constant
      -- no need to recursively process
      return (VariableDclr name mStorage mExpr)

resolveExpr :: ASTExpr -> MapState ASTExpr
resolveExpr expr = case expr of
  ASTAssign left right -> case left of
    (Factor (ASTVar name)) -> do
      rsltLeft <- resolveExpr left
      rsltRight <- resolveExpr right
      return (ASTAssign rsltLeft rsltRight)
    _ -> lift (Left "Semantics Error: Invalid lvalue")
  ASTPostAssign left right -> case left of
    (Factor (ASTVar name)) -> do
      rsltLeft <- resolveExpr left
      rsltRight <- resolveExpr right
      return (ASTPostAssign rsltLeft rsltRight)
    _ -> lift (Left "Semantics Error: Invalid lvalue")
  ASTBinary op left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (ASTBinary op rsltLeft rsltRight)
  Factor fctr -> do
    rslt <- resolveFactor fctr
    return (Factor rslt)
  Conditional condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (Conditional rsltCondition rsltLeft rsltRight)

resolveFactor :: Factor -> MapState Factor
resolveFactor fctr = case fctr of
  ASTLit n -> return (ASTLit n)
  ASTUnary op fctr' -> do
    rslt <- resolveFactor fctr'
    return (ASTUnary op rslt)
  FactorExpr expr -> do
    rslt <- resolveExpr expr
    return (FactorExpr rslt)
  ASTVar name -> do
    maps <- getFst
    case lookup name maps of
      Just (MapEntry newName _ _) -> return (ASTVar newName)
      Nothing -> lift (Left $ "Semantics Error: No declaration for variable " ++ show name)
  FunctionCall name args -> do
    maps <- getFst
    case lookup name maps of
      Just entry -> do
        newArgs <- resolveArgs args
        return (FunctionCall (entryName entry) newArgs)
      Nothing -> lift (Left $ "Semantics Error: Function " ++ show name ++ " has not been declared")

resolveArgs :: [ASTExpr] -> MapState [ASTExpr]
resolveArgs = foldr f (return [])
  where f x xs = do
          expr <- resolveExpr x
          exprs <- xs
          return (expr : exprs)

-- utility functions
getFst :: MonadState (a, b) m => m a
getFst = do
  gets fst

putFst :: MonadState (a, b) m => a -> m ()
putFst maps = do
  (_, n) <- get
  put (maps, n)

getSnd :: MonadState (a, b) m => m b
getSnd = do
  gets snd

putSnd :: MonadState (a, b) m => b -> m ()
putSnd n = do
  (val, _) <- get
  put (val, n)

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

liftMaybe :: Monad m => (a -> StateT b m d) -> Maybe a -> StateT b m (Maybe d)
liftMaybe f Nothing  = return Nothing
liftMaybe f (Just a) = fmap Just (f a)

makeUnique :: Monad m => String -> StateT (a, Int) m String
makeUnique name = do
  n <- getSnd
  putSnd (n + 1)
  return (name ++ "." ++ show n)