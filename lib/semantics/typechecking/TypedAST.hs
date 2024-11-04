module TypedAST(
  module TypedAST,
  CaseLabel, 
  Type_ (..),
  Const_(..), 
  UnaryOp,  
  PostOp, 
  StorageClass(..), 
  BinOp
) 
where

import Utils
import AST(CaseLabel, 
           Type_(..), 
           Const_(..), 
           UnaryOp, 
           PostOp, 
           StorageClass(..), 
           BinOp)

-- typed AST data structure
newtype Prog = Prog [Declaration]

data Declaration = VarDclr VariableDclr | FunDclr FunctionDclr
  deriving (Show)

data VariableDclr = VariableDclr {
  vName :: String,
  vType :: Type_,
  vStorage :: Maybe StorageClass,
  vExpr :: Maybe Expr
}

data BlockItem = StmtBlock Stmt | DclrBlock Declaration

newtype Block = Block [BlockItem]

data ForInit = InitDclr VariableDclr | InitExpr (Maybe Expr)
  deriving (Show)

data Stmt = RetStmt {
             getExpr :: Expr
           }
          | ExprStmt {
             getExpr :: Expr
           }
          | IfStmt {
             getCondition :: Expr, 
             getIfStmt :: Stmt,
             getElseStmt :: Maybe Stmt
           }
          | GoToStmt {
             getLabel :: String
           }
          | LabeledStmt {
             getLabel :: String, 
             getStmt :: Stmt
           }
          | CompoundStmt {
             getBlock :: Block
           }
          | BreakStmt {
             getmLabel :: Maybe String
           }
          | ContinueStmt {
             getmLabel :: Maybe String
           }
          | WhileStmt {
             getCondition :: Expr, 
             getBody :: Stmt, 
             getmLabel :: Maybe String
           }
          | DoWhileStmt {
             getBody :: Stmt, 
             getCondition :: Expr,
             getmLabel :: Maybe String
           }
          | ForStmt {
             getForInit :: ForInit, 
             mCondition :: Maybe Expr,
             getEnd :: Maybe Expr,
             getBody :: Stmt,
             getmLabel :: Maybe String
           }
          | SwitchStmt {
             getExpr :: Expr, 
             getBody :: Stmt,
             getmLabel :: Maybe String, 
             getmCases :: Maybe [CaseLabel]
           } 
          | CaseStmt {
             getExpr :: Expr, 
             getBody :: Stmt, 
             getmLabel :: Maybe String
           }
          | DefaultStmt {
             getBody :: Stmt,
             getmLabel :: Maybe String
           }
          | NullStmt

data Expr = Factor {
             getExprValue :: Factor, 
             getExprType :: Type_
           }
          | Binary {
             getBinOp :: BinOp, 
             getLeftExpr :: Expr,
             getRightExpr :: Expr,
             getExprType :: Type_
           }
          | Assign {
             getDstExpr :: Expr,
             getSrcExpr :: Expr,
             getExprType :: Type_
           }
          | PostAssign {
             getSrc :: Expr, 
             getPostOp :: PostOp,
             getExprType :: Type_
           }
          | Conditional {
             getConditionExpr :: Expr,
             getTrueExpr :: Expr,
             getFalseExpr :: Expr,
             getExprType :: Type_
           }
          deriving (Eq)

data Factor = Lit {
                getConst :: Const_,
                getFactorType :: Type_
            }
            | Unary {
                getUnaryOp :: UnaryOp,
                getFactor :: Factor,
                getFactorType :: Type_
            }
            | FactorExpr {
                getFactorExpr :: Expr,
                getFactorType :: Type_
            }
            | Var {
              getVarName :: String,
              getFactorType :: Type_
            }
            | FunctionCall {
              getFuncName :: String,
              getArgs :: [Expr],
              getFactorType :: Type_
            }
            | Cast {
              getFactorType :: Type_,
              getFactorExpr :: Expr
            }
            deriving (Eq)

data FunctionDclr = FunctionDclr {
  fName :: String, 
  fType :: Type_, 
  fStorage :: Maybe StorageClass, 
  fParams :: [VariableDclr],
  fBody :: Maybe Block
}

-- types used for typechecking:

type SymbolTable = [(String, (Type_, IdentAttrs))]

data IdentAttrs = FunAttr {
                    isDefined :: Bool,
                    isGlobal :: Bool
                  }
                | StaticAttr {
                    init :: IdentInit,
                    isGlobal :: Bool
                  }
                | LocalAttr
                deriving (Show)

data IdentInit = Tentative | Initial StaticInit | NoInit
  deriving (Show, Eq)

data StaticInit = IntInit {getStaticInit :: Int} 
                | UIntInit {getStaticInit :: Int}
  deriving (Show, Eq)

-- print the AST in a (hopefully) readable way
instance Show Prog where
  show (Prog ds) = "Program(\n" ++ unlines (showDeclaration 1 <$> ds) ++ ")"

showFunc :: Int -> FunctionDclr -> String
showFunc n (FunctionDclr name type_ storageClass params (Just (Block body))) =
  tabs ++ "FunctionDef(\n" ++ tabs ++ "    name=\"" ++ name ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ "    params=" ++ show params ++ "\n"++
  tabs ++ "    body=[\n" ++
  unlines (showBlockItem (n + 1) <$> body) ++
  tabs ++ "    ]\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showFunc n (FunctionDclr name type_ storageClass params Nothing) =
  tabs ++ "FunctionDclr(\n" ++ tabs ++ "    name=\"" ++ name ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    params=" ++ show params ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '

showVar :: Int -> VariableDclr -> String
showVar n (VariableDclr v type_ storageClass (Just expr)) =
  tabs ++ "VariableDclr(\n" ++ tabs ++ "    name=\"" ++ v ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ "    init=" ++ show expr ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showVar n (VariableDclr v type_ storageClass Nothing) =
  tabs ++ "VariableDclr(\n" ++ tabs ++ "    name=\"" ++ v ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show VariableDclr where
  show = showVar 0

instance Show FunctionDclr where
  show = showFunc 0

instance Show BlockItem where
  show = showBlockItem 1

showBlockItem :: Int -> BlockItem -> String
showBlockItem n (StmtBlock x) = showStatement (n + 1)  x ++ ","
showBlockItem n (DclrBlock x) = showDeclaration (n + 1) x ++ ","

showDeclaration :: Int -> Declaration -> String
showDeclaration n (FunDclr f) =
  showFunc n f
showDeclaration n (VarDclr v) =
  showVar n v

showStatement :: Int -> Stmt -> String
showStatement n (RetStmt expr) =
  tabs ++ "Return(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (ExprStmt expr) =
  tabs ++ "ExprStmt(" ++ show expr ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (IfStmt expr stmt1 stmt2) =
  tabs ++ "IfStmt(" ++ show expr ++ ",\n" ++
    showStatement (n + 1) stmt1 ++
    maybe "" (\stmt -> ",\n" ++ showStatement (n + 1) stmt) stmt2 ++
    "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n NullStmt =
  tabs ++ "NullStmt"
  where tabs = replicate (4 * n) ' '
showStatement n (GoToStmt label) =
  tabs ++ "GoToStmt " ++ label
  where tabs = replicate (4 * n) ' '
showStatement n (LabeledStmt label stmt) =
  tabs ++ "LabeledStmt " ++ label ++ ":\n" ++ showStatement (n + 1) stmt
  where tabs = replicate (4 * n) ' '
showStatement n (CompoundStmt (Block items)) =
  tabs ++ "CompoundStmt:\n" ++ intercalate "\n" (showBlockItem (n + 1) <$> items)
  where tabs = replicate (4 * n) ' '
showStatement n (BreakStmt label) =
  tabs ++ "BreakStmt(" ++ show label ++")"
  where tabs = replicate (4 * n) ' '
showStatement n (ContinueStmt label) =
  tabs ++ "ContinueStmt(" ++ show label ++")"
  where tabs = replicate (4 * n) ' '
showStatement n (WhileStmt condition body label) =
  tabs ++ "WhileStmt(" ++ show condition ++ ",\n" ++
  showStatement (n + 1) body ++ "\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (DoWhileStmt body condition label) =
  tabs ++ "DoWhileStmt(\n" ++ showStatement (n + 1) body ++ ",\n" ++
  tabs ++ "    " ++ show condition ++ ",\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (ForStmt init_ condition end body label) =
  tabs ++ "ForStmt(" ++ show init_ ++ ",\n" ++
  tabs ++ "    " ++ showMaybe condition ++ ",\n" ++
  tabs ++ "    " ++ showMaybe end ++ ",\n" ++
  showStatement (n + 1) body ++ "\n" ++ tabs ++
  "    " ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (SwitchStmt expr stmt label (Just cases)) =
  tabs ++ "SwitchStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ "\n" ++ tabs ++
  "cases=\n" ++
  intercalate "\n" ((\s -> tabs ++ "    " ++ s) . show <$> cases) ++
  "\n" ++ tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (SwitchStmt expr stmt label Nothing) =
  tabs ++ "SwitchStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ "\n" ++ tabs ++
  "cases=Nothing)"
  where tabs = replicate (4 * n) ' '
showStatement n (CaseStmt expr stmt label) =
  tabs ++ "CaseStmt(" ++ show expr ++ ",\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '
showStatement n (DefaultStmt stmt label) =
  tabs ++ "DefaultStmt(" ++ "\n" ++
  showStatement (n + 1) stmt ++ "\n" ++
  tabs ++ show label ++ ")"
  where tabs = replicate (4 * n) ' '

instance Show Expr where
  show (Factor x _) = show x
  show (Binary op left right type_) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ "," ++ show type_ ++ ")"
  show (Assign x y type_) =
    "Assign(" ++ show x ++ ", " ++ show y ++ "," ++ show type_ ++ ")"
  show (PostAssign x op type_) =
    "PostAssign(" ++ show x ++ ", "++ show op ++ "," ++ show type_ ++ ")"
  show (Conditional c x y type_) =
    "Conditional(" ++ show c ++ ", " ++ show x ++ ", " ++ show y ++ "," ++ show type_ ++ ")"

instance Show Factor where
  show (Lit n _) = show n
  show (Unary op expr type_) = show op ++ "(" ++ show expr ++ "," ++ show type_ ++ ")"
  show (FactorExpr expr _) = show expr
  show (Var x type_) = "Var(" ++ show x ++ "," ++ show type_ ++ ")"
  show (FunctionCall name args type_) =
    "FunctionCall(" ++ name ++ ", " ++ show args ++ "," ++ show type_ ++ ")"
  show (Cast type_ expr) =
    "Cast(" ++ show expr ++ "," ++ show type_ ++ ")"

intStaticInit :: Type_ -> Int -> IdentInit
intStaticInit IntType = Initial . IntInit
intStaticInit UIntType = Initial . UIntInit
intStaticInit _ = error "Compiler Error: invalid static init"

litExpr :: Int -> Type_ -> Expr
litExpr i type_ = Factor (Lit (typeConstructor i) type_) type_
  where typeConstructor = case type_ of
          IntType -> ConstInt
          UIntType -> ConstUInt
          FunType _ _ -> error "Compiler Error: tried to make literal expr with function type"