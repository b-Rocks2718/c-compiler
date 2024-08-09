module Semantics where

import Lexer
import Parser
import Control.Applicative
import Data.List ( foldl', deleteBy )
import Control.Monad.State

type VarMap = [(String, (String, Bool))]
type VarMapState = State (VarMap, Int)
type LabelMap = [(String, String)]

getMaps :: MonadState (a, b) m => m a
getMaps = do
  gets fst

putMaps :: MonadState (a, b) m => a -> m ()
putMaps maps = do
  (_, n) <- get
  put (maps, n)

getN :: MonadState (a, b) m => m b
getN = do
  gets snd

putN :: MonadState (a, b) m => b -> m ()
putN n = do
  (val, _) <- get
  put (val, n)

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

funnyFmap :: (Monad m, Applicative f) => (t -> m (f a)) -> Maybe t -> m (f (Maybe a))
funnyFmap f mX =
  case mX of
    Just x -> do
      rslt <- f x
      return (Just <$> rslt)
    Nothing -> return (pure Nothing)

resolveProg :: ASTProg -> Either String ASTProg
resolveProg (ASTProg p) = ASTProg <$> resolveFunc p

resolveFunc :: ASTFunc -> Either String ASTFunc
resolveFunc (ASTFunc name block) = do
    resolvedVars <- evalState (resolveBlock block) ([], 0)
    maps <- createLabelMaps name resolvedVars
    resolvedLabels <- resolveLabels name maps resolvedVars
    pure $ ASTFunc name resolvedLabels

createLabelMaps :: String -> Block -> Either String LabelMap
createLabelMaps name (Block items) = foldr (createLabelMap name) (pure []) items

createLabelMap :: String -> BlockItem -> Either String LabelMap -> Either String LabelMap
createLabelMap name item eMaps =
    case item of
      StmtBlock (LabeledStmt label stmt) -> do
        maps <- eMaps
        case lookup label maps of
          Just _ -> Left $ "Semantics Error: Multiple definitions for label " ++ show label
          Nothing -> do
            let newLabel = name ++ "." ++ label
            foldr (createLabelMap name) (pure $ (label, newLabel) : maps) [StmtBlock stmt]
      StmtBlock (IfStmt expr stmt1 mStmt2) -> do
        newMaps <- foldr (createLabelMap name) eMaps [StmtBlock stmt1]
        case mStmt2 of
          Just stmt2 -> foldr (createLabelMap name) (pure newMaps) [StmtBlock stmt2]
          Nothing -> pure newMaps
      _ -> eMaps

resolveLabels :: String -> LabelMap -> Block -> Either String Block
resolveLabels name maps (Block items) = foldr (f name maps) (pure $ Block []) items
  where f name maps item eItems = do
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

resolveBlock :: Block -> VarMapState (Either String Block)
resolveBlock (Block items) = do
  rslt <- foldl' resolveBlockItems (return (Right [])) items
  return (Block <$> rslt)

resolveBlockItems :: VarMapState (Either String [BlockItem]) -> BlockItem ->
    VarMapState (Either String [BlockItem])
resolveBlockItems state instr = do
  eInstrs <- state
  case instr of
    StmtBlock x -> do
      rslt <- resolveStmt x
      return (liftA2 append eInstrs (StmtBlock <$> rslt))
    DclrBlock x -> do
      resolved <- resolveDclr x
      return (liftA2 append eInstrs (DclrBlock <$> resolved))

resolveStmt :: ASTStmt -> VarMapState (Either String ASTStmt)
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
    maps <- getMaps
    let newMaps = copyVarMap maps
    putMaps newMaps
    rslt <- resolveBlock block
    putMaps maps
    return (CompoundStmt <$> rslt)
  NullStmt -> return (pure NullStmt)

replace :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
replace x y maps =
  case lookup x maps of
    Just _ -> (x, y) : deleteBy (\(a, _) (a', _) -> a == a') (x, y) maps
    Nothing -> (x, y) : maps

copyVarMap :: VarMap -> VarMap
copyVarMap = foldr (\(x, (y, _)) maps -> (x, (y, False)) : maps) []

resolveDclr :: ASTDclr -> VarMapState (Either String ASTDclr)
resolveDclr (ASTDclr name mExpr) = do
  maps <- getMaps
  case lookup name maps of
    (Just (_, True)) ->
      return (Left $ "Semantics Error: Multiple declarations for " ++ name)
    (Just (_, False)) -> do
      newName <- makeUnique name
      let newMaps = replace name (newName, True) maps
      putMaps newMaps
      newExpr <- funnyFmap resolveExpr mExpr
      return (ASTDclr newName <$> newExpr)
    Nothing -> do
      newName <- makeUnique name
      let newMaps = (name, (newName, True)) : maps
      putMaps newMaps
      newExpr <- funnyFmap resolveExpr mExpr
      return (ASTDclr newName <$> newExpr)

resolveExpr :: ASTExpr -> VarMapState (Either String ASTExpr)
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

resolveFactor :: Factor -> VarMapState (Either String Factor)
resolveFactor fctr = case fctr of
  ASTLit n -> return (Right (ASTLit n))
  ASTUnary op fctr' -> do
    rslt <- resolveFactor fctr'
    return (ASTUnary op <$> rslt)
  FactorExpr expr -> do
    rslt <- resolveExpr expr
    return (FactorExpr <$> rslt)
  ASTVar name -> do
    maps <- getMaps
    case lookup name maps of
      Just (newName, _) -> return (Right $ ASTVar newName)
      Nothing -> return (Left $ "Semantics Error: No declaration for " ++ name)

makeUnique :: String -> VarMapState String
makeUnique name = do
  n <- getN
  putN (n + 1)
  return (name ++ "." ++ show n)