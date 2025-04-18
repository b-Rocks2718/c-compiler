
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
  vExpr :: Maybe VarInit
}

data FunctionDclr = FunctionDclr {
  fName :: String,
  fType :: Type_,
  fStorage :: Maybe StorageClass,
  fParams :: [VariableDclr],
  fBody :: Maybe Block
}

data VarInit = SingleInit Expr Type_
             | CompoundInit [VarInit] Type_

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

data Expr = Binary {
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
          | Lit {
                getConst :: Const_,
                getExprType :: Type_
          }
          | Unary {
              getUnaryOp :: UnaryOp,
              getFactor :: Expr,
              getExprType :: Type_
          }
          | Var {
            getVarName :: String,
            getExprType :: Type_
          }
          | FunctionCall {
            getFuncName :: String,
            getArgs :: [Expr],
            getExprType :: Type_
          }
          | Cast {
            getExprType :: Type_,
            getFactor :: Expr
          }
          | AddrOf {
            getFactor :: Expr,
            getExprType :: Type_
          }
          | Dereference {
            getFactor :: Expr,
            getExprType :: Type_
          }
          | Subscript {
            getFactor1 :: Expr,
            getFactor2 :: Expr,
            getExprType :: Type_
          }
          deriving (Eq)

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

data IdentInit = Tentative | Initial [StaticInit] | NoInit
  deriving (Show, Eq)

data StaticInit = IntInit {getStaticInit :: Int}
                | UIntInit {getStaticInit :: Int}
                | LongInit {getStaticInit :: Int}
                | ULongInit {getStaticInit :: Int}
                | ZeroInit {words :: Int}
  deriving (Eq)

-- print the AST in a (hopefully) readable way
instance Show Prog where
  show (Prog ds) = "Program(\n" ++ unlines (showDeclaration 1 <$> ds) ++ ")"

showFunc :: Int -> FunctionDclr -> String
showFunc n (FunctionDclr name type_ storageClass params (Just (Block body))) =
  tabs ++ "FunctionDef(\n" ++ tabs ++ "    name=\"" ++ name ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ "    params=[\n" ++ concatMap (showParam $ n + 2) params ++
  tabs ++ "    ]\n" ++
  tabs ++ "    body=[\n" ++
  unlines (showBlockItem (n + 1) <$> body) ++
  tabs ++ "    ]\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showFunc n (FunctionDclr name type_ storageClass params Nothing) =
  tabs ++ "FunctionDclr(\n" ++ tabs ++ "    name=\"" ++ name ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    params=[\n" ++ concatMap (showParam $ n + 2) params ++
  tabs ++ "    ]\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '

showParam :: Int -> VariableDclr -> String
showParam n (VariableDclr v type_ storageClass _) =
  tabs ++ "VariableDclr(\n" ++ tabs ++ "    name=\"" ++ v ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ "),\n"
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
  show (Binary op left right type_) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ "," ++ show type_ ++ ")"
  show (Assign x y type_) =
    "Assign(" ++ show x ++ ", " ++ show y ++ "," ++ show type_ ++ ")"
  show (PostAssign x op type_) =
    "PostAssign(" ++ show x ++ ", "++ show op ++ "," ++ show type_ ++ ")"
  show (Conditional c x y type_) =
    "Conditional(" ++ show c ++ ", " ++ show x ++ ", " ++ show y ++ "," ++ show type_ ++ ")"
  show (Lit n _) = show n
  show (Unary op expr type_) = show op ++ "(" ++ show expr ++ "," ++ show type_ ++ ")"
  show (Var x type_) = "Var(" ++ show x ++ "," ++ show type_ ++ ")"
  show (FunctionCall name args type_) =
    "FunctionCall(" ++ name ++ ", " ++ show args ++ "," ++ show type_ ++ ")"
  show (Cast type_ expr) =
    "Cast(" ++ show expr ++ "," ++ show type_ ++ ")"
  show (AddrOf expr type_) =
    "AddrOf(" ++ show expr ++ ", " ++ show type_ ++ ")"
  show (Dereference expr type_) = 
    "Dereference(" ++ show expr ++ ", " ++ show type_ ++ ")"
  show (Subscript left right type_) =
    "Subscript(" ++ show left ++ ", " ++ show right ++ ", " ++ show type_ ++ ")"

instance Show StaticInit where
  show (ZeroInit n) = ".space " ++ show n
  show init_ = show $ getStaticInit init_

intStaticInit :: Type_ -> Int -> [StaticInit]
intStaticInit IntType n = [IntInit n]
intStaticInit UIntType n = [UIntInit n]
intStaticInit LongType n = [LongInit n]
intStaticInit ULongType n = [ULongInit n]
intStaticInit (PointerType _) n = [UIntInit n]
intStaticInit _ _ = error "Compiler Error: invalid integer static init"

litExpr :: Int -> Type_ -> Expr
litExpr i type_ = Lit (typeConstructor i) type_
  where typeConstructor = case type_ of
          IntType -> ConstInt
          LongType -> ConstLong
          UIntType -> ConstUInt
          ULongType -> ConstULong
          PointerType _ -> ConstUInt
          FunType _ _ -> error "Compiler Error: tried to make literal expr with function type"
          ArrayType _ _ -> error "Compiler Error: tried to make literal expr with array type"

instance Show IdentAttrs where
  show (FunAttr def global) = 
    "FunAttr: defined=" ++ show def ++ ", global=" ++ show global
  show (StaticAttr init_ global) = 
    "StaticAttr: init=" ++ show init_ ++ ", global=" ++ show global
  show LocalAttr = "LocalAttr"

instance Show VarInit where
  show (SingleInit expr _) = show expr
  show (CompoundInit inits _) =
    "{" ++ intercalate "," (show <$> inits) ++ "}"