module Semantics where

import Lexer
import Parser
import Control.Applicative
import Data.List ( foldl' )

type VarMap = [(String, String)]
type VarMapState = (VarMap, Int)
type LabelMap = [(String, String)]

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

resolveProg :: ASTProg -> Either String ASTProg
resolveProg (ASTProg p) = ASTProg <$> resolveFunc p

resolveFunc :: ASTFunc -> Either String ASTFunc
resolveFunc (ASTFunc name items) = do
    resolvedVars <- fst (f items)
    maps <- createLabelMaps name resolvedVars
    resolvedLabels <- resolveLabels name maps resolvedVars
    pure $ ASTFunc name resolvedLabels
  where f = foldl' resolveBlockItems (Right [], ([], 0))

createLabelMaps :: String -> [BlockItem] -> Either String LabelMap
createLabelMaps name = foldr (createLabelMap name) (pure [])

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

resolveLabels :: String -> LabelMap -> [BlockItem] -> Either String [BlockItem]
resolveLabels name maps = foldr (f name maps) (pure [])
  where f name maps item eItems = do
          items <- eItems
          case resolveBlockItemsLabels name maps item of
            Right resolved -> return (resolved : items)
            Left s -> Left s

resolveBlockItemsLabels :: String -> LabelMap -> BlockItem -> Either String BlockItem
resolveBlockItemsLabels name maps item = 
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

resolveBlockItems :: (Either String [BlockItem], VarMapState) -> BlockItem ->
    (Either String [BlockItem], VarMapState)
resolveBlockItems (instrs, (maps, n)) instr = case instr of
  StmtBlock x -> let resolved = resolveStmt maps x
                 in (liftA2 append instrs (StmtBlock <$> resolved), (maps, n))
  DclrBlock x -> let (resolved, (newMaps, n')) = resolveDclr maps n x
                 in (liftA2 append instrs (DclrBlock <$> resolved), (newMaps, n'))

resolveStmt :: VarMap -> ASTStmt -> Either String ASTStmt
resolveStmt maps stmt = case stmt of
  RetStmt expr -> RetStmt <$> resolveExpr maps expr
  ExprStmt expr -> ExprStmt <$> resolveExpr maps expr
  IfStmt condition left right ->
    liftA3 IfStmt (resolveExpr maps condition)
    (resolveStmt maps left)
    (funnyFmap (resolveStmt maps) right)
  LabeledStmt label stmt -> LabeledStmt label <$> resolveStmt maps stmt
  GoToStmt label -> pure (GoToStmt label)
  NullStmt -> pure NullStmt

funnyFmap :: (a -> Either b c) -> Maybe a -> Either b (Maybe c)
funnyFmap f x =
  case x of
    Just x -> case f x of
      Left s -> Left s
      Right y -> Right (Just y)
    Nothing -> pure Nothing

resolveDclr :: VarMap -> Int -> ASTDclr -> (Either String ASTDclr, VarMapState)
resolveDclr maps n dclr@(ASTDclr name expr) =
  case lookup name maps of
    (Just _) -> (Left $ "Semantics Error: Multiple declarations for " ++ name, (maps, n))
    Nothing -> let newName = makeUnique name n
                   newMaps = (name, newName) : maps
                   newExpr = funnyFmap (resolveExpr maps) expr
               in (ASTDclr newName <$> newExpr, (newMaps, n + 1))

resolveExpr :: VarMap -> ASTExpr -> Either String ASTExpr
resolveExpr maps expr = case expr of
  ASTAssign left right -> case left of
    (Factor (ASTVar name)) ->
      liftA2 ASTAssign (resolveExpr maps left) (resolveExpr maps right)
    _ -> Left "Semantics Error: Invalid lvalue"
  ASTPostAssign left right -> case left of
    (Factor (ASTVar name)) ->
      liftA2 ASTPostAssign (resolveExpr maps left) (resolveExpr maps right)
    _ -> Left "Semantics Error: Invalid lvalue"
  ASTBinary op left right ->
    liftA2 (ASTBinary op) (resolveExpr maps left) (resolveExpr maps right)
  Factor fctr -> Factor <$> resolveFactor maps fctr
  Conditional condition left right ->
    liftA3 Conditional
    (resolveExpr maps condition) (resolveExpr maps left) (resolveExpr maps right)

resolveFactor :: VarMap -> Factor -> Either String Factor
resolveFactor maps fctr = case fctr of
  ASTLit n -> Right (ASTLit n)
  ASTUnary op fctr' -> ASTUnary op <$> resolveFactor maps fctr'
  FactorExpr expr -> FactorExpr <$> resolveExpr maps expr
  ASTVar name -> case lookup name maps of
    Just newName -> Right (ASTVar newName)
    Nothing -> Left $ "Semantics Error: No declaration for " ++ name

makeUnique :: String -> Int -> String
makeUnique name n = name ++ "." ++ show n