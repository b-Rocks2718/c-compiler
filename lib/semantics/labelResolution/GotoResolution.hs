
module GotoResolution where

import Utils
import AST

type LabelMap = [(String, String)]

resolveGotos :: Declaration -> Result Declaration
resolveGotos dclr = 
  case dclr of
    FunDclr f -> do
      maps <- createLabelMaps f
      FunDclr <$> resolveLabels maps f
    VarDclr _ -> pure dclr

createLabelMaps :: FunctionDclr -> Result LabelMap
createLabelMaps (FunctionDclr name _ _ _ (Just (Block items))) =
  foldr (createLabelMap name) (pure []) items
createLabelMaps (FunctionDclr _ _ _ _ Nothing) = pure []

createLabelMap :: String -> BlockItem -> Result LabelMap -> Result LabelMap
createLabelMap name item eMaps =
  case item of
    DclrBlock _ -> eMaps -- can't label declarations, only statements
    StmtBlock stmt ->
      case stmt of
        -- if it's a labeled statement, add the label to the label map
        LabeledStmt label stmt' -> do
          maps <- eMaps
          case lookup label maps of
            -- ensure there's no duplicate labels
            Just _ -> Err $ "Semantics Error: Multiple definitions for label " ++ show label
            Nothing -> do
              -- make a name unique to this function
              let newLabel = name ++ "." ++ label
              createLabelMap name (StmtBlock stmt') (pure $ (label, newLabel) : maps)

        -- recursively check for more labeled statements
        IfStmt _ stmt1 mStmt2 -> do
          newMaps <- createLabelMap name (StmtBlock stmt1) eMaps
          case mStmt2 of
            Just stmt2 -> createLabelMap name (StmtBlock stmt2) (pure newMaps)
            Nothing -> pure newMaps
        CompoundStmt (Block items) -> foldr (createLabelMap name) eMaps items
        WhileStmt _ stmt' _ -> createLabelMap name (StmtBlock stmt') eMaps
        DoWhileStmt stmt' _ _ -> createLabelMap name (StmtBlock stmt') eMaps
        ForStmt _ _ _ stmt' _ -> createLabelMap name (StmtBlock stmt') eMaps
        SwitchStmt _ stmt' _ _ -> createLabelMap name (StmtBlock stmt') eMaps
        CaseStmt _ stmt' _ ->  createLabelMap name (StmtBlock stmt') eMaps
        DefaultStmt stmt' _ ->  createLabelMap name (StmtBlock stmt') eMaps
        _ -> eMaps -- other statements are not recursive

resolveLabels :: LabelMap -> FunctionDclr -> Result FunctionDclr
resolveLabels maps (FunctionDclr name type_ mStorage params (Just (Block items))) =
  FunctionDclr name type_ mStorage params . Just <$> foldr (resolveBlockLabels name maps) (pure $ Block []) items
resolveLabels _ f@(FunctionDclr _ _ _ _ Nothing) =
  pure f

resolveBlockLabels :: String -> LabelMap -> BlockItem -> Result Block -> Result Block
resolveBlockLabels name maps item eItems = do
  (Block items) <- eItems
  case resolveItemLabels name maps item of
    Ok resolved -> return (Block $ resolved : items)
    Err s -> Err s
    Fail -> Fail

resolveItemLabels :: String -> LabelMap -> BlockItem -> Result BlockItem
resolveItemLabels name maps item =
  case item of
    DclrBlock _ -> return item
    StmtBlock stmt -> StmtBlock <$> resolveLabel name maps stmt

resolveLabel :: String -> LabelMap -> Stmt -> Result Stmt
resolveLabel name maps item =
    case item of
      -- ensure there's a label to jump to
      GoToStmt label ->
        case lookup label maps of
          Just newLabel -> return (GoToStmt newLabel)
          Nothing -> Err $ "Semantics Error: Label " ++ show label ++ " has no definition"

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
      ForStmt init_ condition end stmt label ->
        lift5 ForStmt (pure init_) (pure condition) (pure end) (resolveLabel name maps stmt) (pure label)
      SwitchStmt expr stmt label cases -> lift4 SwitchStmt (pure expr) (resolveLabel name maps stmt) (pure label) (pure cases)
      CaseStmt expr stmt label -> liftA3 CaseStmt (pure expr) (resolveLabel name maps stmt) (pure label)
      DefaultStmt stmt label -> liftA2 DefaultStmt (resolveLabel name maps stmt) (pure label)
      _ -> return item