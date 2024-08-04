module Semantics where

import Lexer
import Parser
import Control.Applicative
import Data.List ( foldl' )

type VarMap = [(String, String)]
type MapState = (VarMap, Int)

append :: [a] -> a -> [a]
append xs x = xs ++ [x]

resolveProg :: ASTProg -> Either String ASTProg
resolveProg (ASTProg p) = ASTProg <$> resolveFunc p

resolveFunc :: ASTFunc -> Either String ASTFunc
resolveFunc (ASTFunc name items) = ASTFunc name <$>
  fst (foldl' resolveBlockItems (Right [], ([], 0)) items)

resolveBlockItems :: (Either String [BlockItem], MapState) -> BlockItem -> 
    (Either String [BlockItem], MapState)
resolveBlockItems (instrs, (maps, n)) instr = case instr of
    StmtBlock x -> let resolved = resolveStmt maps x
                   in (liftA2 append instrs (StmtBlock <$> resolved), (maps, n))
    DclrBlock x -> let (resolved, (newMaps, n')) = resolveDclr maps n x
                   in (liftA2 append instrs (DclrBlock <$> resolved), (newMaps, n'))

resolveStmt :: VarMap -> ASTStmt -> Either String ASTStmt
resolveStmt maps stmt = case stmt of
    ASTReturn expr -> ASTReturn <$> resolveExpr maps expr
    ExprStmt expr -> ExprStmt <$> resolveExpr maps expr
    NullStmt -> pure NullStmt

resolveDclr :: VarMap -> Int -> ASTDclr -> (Either String ASTDclr, MapState)
resolveDclr maps n dclr@(ASTDclr name expr) = 
  case lookup name maps of
    (Just _) -> (Left $ "Semantics Error: Multiple declarations for " ++ name, (maps, n))
    Nothing -> let newName = makeUnique name n
                   newMaps = (name, newName) : maps
                   newExpr = case expr of -- funny fmap
                        Just x -> case resolveExpr maps x of
                          Left s -> Left s
                          Right y -> Right (Just y)
                        Nothing -> pure Nothing
               in (ASTDclr newName <$> newExpr, ((name, newName) : maps, n + 1))

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