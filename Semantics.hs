{-# LANGUAGE FlexibleContexts #-}

module Semantics where

import Lexer
import Parser
import Control.Applicative
import Data.List ( foldl', deleteBy )
import Data.List.Split
import Control.Monad.State
import Control.Applicative.HT (lift5)


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

-- there's probably a better name for this function
funnyFmap :: (Monad m, Applicative f) => (t -> m (f a)) -> Maybe t -> m (f (Maybe a))
funnyFmap f mX =
  case mX of
    Just x -> do
      rslt <- f x
      return (Just <$> rslt)
    Nothing -> return (pure Nothing)

resolveProg :: ASTProg -> Either String ASTProg
resolveProg (ASTProg p) = do 
  rslt <- ASTProg <$> resolveProgFold p
  evalState (typecheck rslt) []
  return rslt

resolveProgFold :: [FunctionDclr] -> Either String [FunctionDclr]
resolveProgFold fs = evalState (foldr f (return . pure $ []) fs) ([], 0)
  where f x maps = do
          rslt <- resolveFunc x
          liftA2 (:) rslt <$> maps

-------------- goto/labeled statement resolution ----------------

type LabelMap = [(String, String)]

createLabelMaps :: String -> FunctionDclr -> Either String LabelMap
createLabelMaps name (FunctionDclr _ _ (Just (Block items))) =
  foldr (createLabelMap name) (pure []) items
createLabelMaps name (FunctionDclr _ _ Nothing) = pure []

createLabelMap :: String -> BlockItem -> Either String LabelMap -> Either String LabelMap
createLabelMap name item eMaps =
    case item of
      StmtBlock (LabeledStmt label stmt) -> do
        maps <- eMaps
        case lookup label maps of
          Just _ -> Left $ "Semantics Error: Multiple definitions for label " ++ show label
          Nothing -> do
            let newLabel = name ++ "." ++ label
            createLabelMap name (StmtBlock stmt) (pure $ (label, newLabel) : maps)
      StmtBlock (IfStmt expr stmt1 mStmt2) -> do
        newMaps <- createLabelMap name (StmtBlock stmt1) eMaps
        case mStmt2 of
          Just stmt2 -> createLabelMap name (StmtBlock stmt2) (pure newMaps)
          Nothing -> pure newMaps
      _ -> eMaps

resolveLabels :: String -> LabelMap -> FunctionDclr -> Either String FunctionDclr
resolveLabels name maps (FunctionDclr fName params (Just (Block items))) =
  FunctionDclr fName params . Just <$> foldr (f name maps) (pure $ Block []) items
  where f name maps item eItems = do
          (Block items) <- eItems
          case resolveItemLabels name maps item of
            Right resolved -> return (Block $ resolved : items)
            Left s -> Left s
resolveLabels name maps f@(FunctionDclr _ _ Nothing) =
  pure f

resolveItemLabels :: String -> LabelMap -> BlockItem -> Either String BlockItem
resolveItemLabels name maps item =
  case item of
    DclrBlock _ -> return item
    StmtBlock stmt -> StmtBlock <$> resolveLabel name maps stmt

resolveLabel :: String -> LabelMap -> ASTStmt -> Either String ASTStmt
resolveLabel name maps item =
    case item of
      (LabeledStmt label stmt) ->
        case lookup label maps of
          Just newLabel -> LabeledStmt newLabel <$> resolveLabel name maps stmt
          Nothing -> error "Something has gone terribly wrong"
      (GoToStmt label) ->
        case lookup label maps of
          Just newLabel -> return (GoToStmt newLabel)
          Nothing -> Left $ "Semantics Error: Label " ++ show label ++ " has no definition"
      (IfStmt expr stmt1 mStmt2) -> do
        resolved1 <- resolveLabel name maps stmt1
        case mStmt2 of
          Just stmt2 -> do
            resolved2 <- resolveLabel name maps stmt2
            return (IfStmt expr resolved1 (Just resolved2))
          Nothing -> return (IfStmt expr resolved1 Nothing)
      _ -> return item

-------------------- Loop Labeling -----------------------------------

labelStmt :: ASTStmt -> State (Maybe String, Int) (Either String ASTStmt)
labelStmt stmt = case stmt of
  BreakStmt _ -> do
    mLabel <- getFst
    case mLabel of
      Nothing -> return (Left "Semantics Error: Break statement outside loop")
      Just label -> return (Right $ BreakStmt mLabel)
  ContinueStmt _ -> do
    mLabel <- getFst
    case mLabel of
      Nothing -> return (Left "Semantics Error: Continue statement outside loop")
      Just label -> return (Right $ ContinueStmt mLabel)
  WhileStmt condition body _ -> do
    label <- makeUnique "while"
    oldLabel <- getFst
    putFst (Just label)
    labeledBody <- labelStmt body
    putFst oldLabel
    return (liftA3 WhileStmt (pure condition) labeledBody (pure . Just $ label))
  DoWhileStmt body condition _ -> do
    label <- makeUnique "doWhile"
    oldLabel <- getFst
    putFst (Just label)
    labeledBody <- labelStmt body
    putFst oldLabel
    return (liftA3 DoWhileStmt labeledBody (pure condition) (pure . Just $ label))
  ForStmt init condition end body _ -> do
    label <- makeUnique "for"
    oldLabel <- getFst
    putFst (Just label)
    labeledBody <- labelStmt body
    putFst oldLabel
    return (lift5 ForStmt (pure init) (pure condition) (pure end)
      labeledBody (pure . Just $ label))
  IfStmt condition left right -> do
    labeledLeft <- labelStmt left
    labeledRight <- funnyFmap labelStmt right
    return (liftA3 IfStmt (pure condition) labeledLeft labeledRight)
  CompoundStmt block -> do
    labeledBlock <- labelBlock block
    return (CompoundStmt <$> labeledBlock)
  LabeledStmt s stmt -> do
    labeledStmt <- labelStmt stmt
    return (LabeledStmt s <$> labeledStmt)
  stmt -> return (pure stmt)

labelBlockItem :: BlockItem -> State (Maybe String, Int) (Either String BlockItem)
labelBlockItem item = case item of
  StmtBlock stmt -> do
    rslt <-labelStmt stmt
    return (StmtBlock <$> rslt)
  DclrBlock dclr -> return (pure $ DclrBlock dclr)

labelBlock :: Block -> State (Maybe String, Int) (Either String Block)
labelBlock (Block items) =  do
    -- BlockItem ->
    --  State (Maybe String, Int) (Either String [BlockItem]) ->
    --  State (Maybe String, Int) (Either String [BlockItem])
    labeledItems <- foldr (liftA2 (liftA2 (:)) . labelBlockItem)
      (return . pure $ []) items
    return (Block <$> labeledItems)

-------------------- Type Checking -------------------------------------

data Type_ = IntType | FunType Int -- param count
  deriving (Show, Eq)
type SymbolTable = [(String, (Type_, Bool))] -- Bool: already_defined

typecheck :: ASTProg -> State SymbolTable (Either String ())
typecheck (ASTProg p) = do
  foldl' typecheckFold (return $ pure ()) p

typecheckFold :: State SymbolTable (Either String ()) ->  FunctionDclr -> 
  State SymbolTable (Either String ())
typecheckFold oldState f = do
  -- set maps to whatever they were in the previous state
  rslt <- oldState
  maps <- do
    oldState
    get
  put maps
  -- typecheck next item (as long as previous check succeeded)
  case rslt of
    Right () -> typecheckFunc f
    Left s -> return (Left s)

typecheckFunc :: FunctionDclr -> State SymbolTable (Either String ())
typecheckFunc (FunctionDclr name params mBody) = do
  let funType = FunType $ length params
  let hasBody = case mBody of
        Just _ -> True
        Nothing -> False
  maps <- get
  checkDclr <- case lookup name maps of
    Just (oldType, defined) -> 
      if oldType /= funType
        then return (Left $ "Incompatible function declarations for " ++ show name)
      else if hasBody && defined
        then return (Left $ "Multiple definitions for function " ++ show name)
      else return (pure ())
    Nothing -> do
      put $ (name, (funType, hasBody)) : maps
      return (pure ())
  case checkDclr of
    Left s -> return (Left s)
    Right () -> case mBody of
      Just body -> do
        typecheckParams params
        typecheckBlock body
      Nothing -> return (pure ())

typecheckParams :: [VariableDclr] -> State SymbolTable (Either String ())
typecheckParams params = do
  maps <- get
  put $ maps ++ paramMaps params
  return (pure ())

paramMaps :: [VariableDclr] -> SymbolTable
paramMaps = foldr paramMapsFold []

paramMapsFold :: VariableDclr -> SymbolTable -> SymbolTable
paramMapsFold (VariableDclr name Nothing) table = 
  (name, (IntType, False)) : table
paramMapsFold _ table = error "Yikes!"

typecheckBlock :: Block -> State SymbolTable (Either String ())
typecheckBlock (Block items) = foldl' typecheckBlockFold (return (pure ())) items

typecheckBlockFold :: State SymbolTable (Either String ()) -> BlockItem ->
  State SymbolTable (Either String ())
typecheckBlockFold oldState item = oldState >?> typecheckBlockItem item

typecheckBlockItem :: BlockItem -> State SymbolTable (Either String ())
typecheckBlockItem item = case item of
  StmtBlock stmt -> typecheckStmt stmt
  DclrBlock dclr -> typecheckDclr dclr

typecheckStmt :: ASTStmt -> State SymbolTable (Either String ())
typecheckStmt stmt = case stmt of
  RetStmt expr -> typecheckExpr expr
  ExprStmt expr -> typecheckExpr expr
  IfStmt expr stmt1 mStmt2 -> 
    funnyFmap typecheckStmt mStmt2 >?>
    typecheckExpr expr >?>
    typecheckStmt stmt1
  GoToStmt label -> return (pure ())
  LabeledStmt name stmt -> typecheckStmt stmt
  CompoundStmt block -> typecheckBlock block
  BreakStmt _ -> return (pure ())
  ContinueStmt _ -> return (pure ())
  WhileStmt expr stmt _ -> typecheckExpr expr >?> typecheckStmt stmt
  DoWhileStmt stmt expr _ -> typecheckStmt stmt >?> typecheckExpr expr
  ForStmt init mExpr1 mExpr2 stmt _ -> 
    funnyFmap typecheckExpr mExpr1 >?>
    funnyFmap typecheckExpr mExpr2 >?>
    typecheckForInit init >?>
    typecheckStmt stmt
  NullStmt -> return (pure ())

typecheckForInit :: ForInit -> State SymbolTable (Either String ())
typecheckForInit init = case init of
  InitDclr dclr -> typecheckVar dclr
  InitExpr (Just expr) -> typecheckExpr expr
  InitExpr Nothing -> return (pure ())

typecheckDclr :: ASTDclr -> State SymbolTable (Either String ())
typecheckDclr dclr = case dclr of
  VarDclr v -> typecheckVar v
  FunDclr f -> typecheckFunc f

typecheckVar :: VariableDclr -> State SymbolTable (Either String ())
typecheckVar (VariableDclr v mExpr) = do
  maps <- get
  put ((v, (IntType, False)) : maps)
  case mExpr of
    Nothing -> return $ pure ()
    Just expr -> typecheckExpr expr

typecheckExpr :: ASTExpr -> State SymbolTable (Either String ())
typecheckExpr e = case e of
  Factor f -> typecheckFactor f
  ASTBinary _ left right -> typecheckExpr left >?> typecheckExpr right
  ASTAssign left right -> typecheckExpr left >?> typecheckExpr right
  ASTPostAssign left right -> typecheckExpr left >?> typecheckExpr right
  Conditional c left right -> typecheckExpr c >?> typecheckExpr left >?> typecheckExpr right

typecheckFactor :: Factor -> State SymbolTable (Either String ())
typecheckFactor f = case f of
  FunctionCall name args -> do
    maps <- get
    case lookup name maps of
      Just (rsltType, _) -> case rsltType of
        IntType -> return 
          (Left $ "Variable " ++ show (head $ splitOn "." name) ++ " cannot be used as a function")
        FunType n -> if n == length args 
          then typecheckArgs args
          else return (Left $ "Function " ++ show name ++ " called with wrong number of arguments") 
      Nothing -> error "That should not happen"
  ASTVar v -> do
    maps <- get
    case lookup v maps of
      Just (rsltType, _) -> case rsltType of
        IntType -> return (pure ())
        FunType _ -> return (Left $ "Function " ++ show v ++ "cannot be used as a variable")
      Nothing -> error "That should not happen"
  ASTUnary _ f -> typecheckFactor f
  FactorExpr expr -> typecheckExpr expr
  ASTLit _ -> return (pure ())

typecheckArgs :: [ASTExpr] -> State SymbolTable (Either String ())
typecheckArgs = 
  foldr (\x rslt -> rslt >?> typecheckExpr x) (return . pure $ ())

-- normal '>>' operator ignores Left values when they are wrapped in the State monad
-- my '>?>' operator will propogate Left values 
funnyAndThen :: Monad m => m (Either a b) -> m (Either a c) -> m (Either a c)
funnyAndThen x y = do
  x' <- x
  case x' of
    Left s -> return (Left s)
    Right _ -> y

(>?>) :: Monad m => m (Either a b) -> m (Either a c) -> m (Either a c)
(>?>) = funnyAndThen

-------------------- Identifier Resolution -----------------------------

data Linkage = None | Internal | External
data MapEntry = MapEntry {
  entryName :: String,
  fromCurrentScope :: Bool,
  linkage :: Linkage
}
type IdentMap = [(String, MapEntry)]
type MapState = State (IdentMap, Int)

resolveFunc :: FunctionDclr -> MapState (Either String FunctionDclr)
resolveFunc (FunctionDclr name params body) = do
  maps <- getFst
  resolvedFunc <- case lookup name maps of
    (Just (MapEntry _ True None)) ->
      return (Left $ "Semantics Error: Multiple declarations for " ++ name)
    (Just (MapEntry newName scope link)) -> do
      let newMaps = replace name (MapEntry newName True External) maps
      putFst newMaps
      newParams <- resolveParams params
      newBody <- funnyFmap resolveBlock body
      return (liftA2 (FunctionDclr newName) newParams newBody)
    Nothing -> do
      let newMaps = (name, MapEntry name True External) : maps
      let newerMaps = copyVarMap newMaps
      putFst newerMaps
      newParams <- resolveParams params
      rslt <- funnyFmap resolveBlock body
      putFst newMaps
      return (liftA2 (FunctionDclr name) newParams rslt)
  let labeledLoops = do
        (FunctionDclr newName newParams newBody) <- resolvedFunc
        FunctionDclr newName newParams <$>
          evalState (funnyFmap labelBlock newBody) (Nothing, 0)
      resolvedLabels = do
        rslt <- labeledLoops
        maps <- createLabelMaps name rslt
        resolveLabels name maps rslt
  return resolvedLabels


resolveLocalFunc :: FunctionDclr -> MapState (Either String FunctionDclr)
resolveLocalFunc (FunctionDclr name params body) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True None)) ->
      return (Left $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry newName scope link)) -> do
      let newMaps = replace name (MapEntry newName True External) maps
      putFst newMaps
      case body of
        Just b ->
          return (Left $ "Semantics Error: Local function definition for " ++ show name)
        Nothing -> return (pure $ FunctionDclr newName params body)
    Nothing -> do
      let newMaps = (name, MapEntry name True External) : maps
      let newerMaps = copyVarMap newMaps
      putFst newerMaps
      newParams <- resolveParams params
      rslt <- funnyFmap resolveBlock body
      putFst newMaps
      case body of
        Just b ->
          return (Left $ "Semantics Error: Local function definition for " ++ show name)
        Nothing -> return (pure $ FunctionDclr name params body)

resolveParams :: [VariableDclr] -> MapState (Either String [VariableDclr])
resolveParams = foldr f (return . pure $ [])
  where f x xs = do
          rslt <- resolveVarDclr x
          liftA2 (:) rslt <$> xs

resolveBlock :: Block -> MapState (Either String Block)
resolveBlock (Block items) = do
  rslt <- foldl' resolveBlockItems (return (Right [])) items
  return (Block <$> rslt)

resolveBlockItems :: MapState (Either String [BlockItem]) -> BlockItem ->
    MapState (Either String [BlockItem])
resolveBlockItems oldState instr = do
  eInstrs <- oldState
  case instr of
    StmtBlock x -> do
      rslt <- resolveStmt x
      return (liftA2 append eInstrs (StmtBlock <$> rslt))
    DclrBlock x -> do
      resolved <- resolveDclr x
      return (liftA2 append eInstrs (DclrBlock <$> resolved))

resolveStmt :: ASTStmt -> MapState (Either String ASTStmt)
resolveStmt stmt = case stmt of
  RetStmt expr -> do
    rslt <- resolveExpr expr
    return (RetStmt <$> rslt)
  ExprStmt expr -> do
    rslt <- resolveExpr expr
    return (ExprStmt <$> rslt)
  IfStmt condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveStmt left
    rsltRight <- funnyFmap resolveStmt right
    return (liftA3 IfStmt rsltCondition rsltLeft rsltRight)
  LabeledStmt label stmt -> do
    rslt <- resolveStmt stmt
    return (LabeledStmt label <$> rslt )
  GoToStmt label -> return (pure $ GoToStmt label)
  CompoundStmt block -> do
    maps <- getFst
    let newMaps = copyVarMap maps
    putFst newMaps
    rslt <- resolveBlock block
    putFst maps
    return (CompoundStmt <$> rslt)
  BreakStmt label -> return (pure $ BreakStmt label)
  ContinueStmt label -> return (pure $ ContinueStmt label)
  WhileStmt condition body label -> do
    rsltCondition <- resolveExpr condition
    rsltBody <- resolveStmt body
    return (liftA3 WhileStmt rsltCondition rsltBody (pure label))
  DoWhileStmt body condition label -> do
    rsltBody <- resolveStmt body
    rsltCondition <- resolveExpr condition
    return (liftA3 DoWhileStmt rsltBody rsltCondition (pure label))
  ForStmt init condition end body label -> do
    maps <- getFst
    let newMaps = copyVarMap maps
    putFst newMaps
    rsltInit <- resolveInit init
    rsltCondition <- funnyFmap resolveExpr condition
    rsltEnd <- funnyFmap resolveExpr end
    rsltBody <- resolveStmt body
    putFst maps
    return (lift5 ForStmt rsltInit rsltCondition rsltEnd rsltBody (pure label))
  NullStmt -> return (pure NullStmt)

resolveInit :: ForInit -> MapState (Either String ForInit)
resolveInit init = case init of
  InitDclr d -> do
    rslt <- resolveVarDclr d
    return (InitDclr <$> rslt)
  InitExpr e -> do
    rslt <- funnyFmap resolveExpr e
    return (InitExpr <$> rslt)

replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace x y maps =
  case lookup x maps of
    Just _ -> (x, y) : deleteBy (\(a, _) (a', _) -> a == a') (x, y) maps
    Nothing -> (x, y) : maps

copyVarMap :: IdentMap -> IdentMap
copyVarMap = foldr (\(s, MapEntry s' _ l) maps ->
  (s, MapEntry s' False l) : maps) []

resolveDclr :: ASTDclr -> MapState (Either String ASTDclr)
resolveDclr dclr = case dclr of
  VarDclr v -> do
    rslt <- resolveVarDclr v
    return (VarDclr <$> rslt)
  FunDclr f -> do
    rslt <- resolveLocalFunc f
    return (FunDclr <$> rslt)

resolveVarDclr :: VariableDclr -> MapState (Either String VariableDclr)
resolveVarDclr (VariableDclr name mExpr) = do
  maps <- getFst
  case lookup name maps of
    (Just (MapEntry _ True _)) ->
      return (Left $ "Semantics Error: Multiple declarations for variable " ++ show name)
    (Just (MapEntry _ False link)) -> do
      newName <- makeUnique name
      let newMaps = replace name (MapEntry newName True link) maps
      putFst newMaps
      newExpr <- funnyFmap resolveExpr mExpr
      return (VariableDclr newName <$> newExpr)
    Nothing -> do
      newName <- makeUnique name
      let newMaps = (name, MapEntry newName True None) : maps
      putFst newMaps
      newExpr <- funnyFmap resolveExpr mExpr
      return (VariableDclr newName <$> newExpr)

resolveExpr :: ASTExpr -> MapState (Either String ASTExpr)
resolveExpr expr = case expr of
  ASTAssign left right -> case left of
    (Factor (ASTVar name)) -> do
      rsltLeft <- resolveExpr left
      rsltRight <- resolveExpr right
      return (liftA2 ASTAssign rsltLeft rsltRight)
    _ -> return (Left "Semantics Error: Invalid lvalue")
  ASTPostAssign left right -> case left of
    (Factor (ASTVar name)) -> do
      rsltLeft <- resolveExpr left
      rsltRight <- resolveExpr right
      return (liftA2 ASTPostAssign rsltLeft rsltRight)
    _ -> return (Left "Semantics Error: Invalid lvalue")
  ASTBinary op left right -> do
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (liftA2 (ASTBinary op) rsltLeft rsltRight)
  Factor fctr -> do
    rslt <- resolveFactor fctr
    return (Factor <$> rslt)
  Conditional condition left right -> do
    rsltCondition <- resolveExpr condition
    rsltLeft <- resolveExpr left
    rsltRight <- resolveExpr right
    return (liftA3 Conditional rsltCondition rsltLeft rsltRight)

resolveFactor :: Factor -> MapState (Either String Factor)
resolveFactor fctr = case fctr of
  ASTLit n -> return (Right (ASTLit n))
  ASTUnary op fctr' -> do
    rslt <- resolveFactor fctr'
    return (ASTUnary op <$> rslt)
  FactorExpr expr -> do
    rslt <- resolveExpr expr
    return (FactorExpr <$> rslt)
  ASTVar name -> do
    maps <- getFst
    case lookup name maps of
      Just (MapEntry newName _ _) -> return (Right $ ASTVar newName)
      Nothing -> return (Left $ "Semantics Error: No declaration for variable " ++ show name)
  FunctionCall name args -> do
    maps <- getFst
    case lookup name maps of
      Just entry -> do
        newArgs <- resolveArgs args
        return (FunctionCall (entryName entry) <$> newArgs)
      Nothing -> return (Left $ "Semantics Error: Function " ++ show name ++ " has not been declared")

resolveArgs :: [ASTExpr] -> MapState (Either String [ASTExpr])
resolveArgs = foldr f (return . pure $ [])
  where f x xs = do
          rslt <- resolveExpr x
          liftA2 (:) rslt <$> xs

makeUnique :: String -> State (a, Int) String
makeUnique name = do
  n <- getSnd
  putSnd (n + 1)
  return (name ++ "." ++ show n)