module AST where

import Utils

-- AST data structure
newtype Prog = Prog [Declaration]

data BlockItem = StmtBlock Stmt | DclrBlock Declaration

newtype Block = Block [BlockItem]

data Stmt = RetStmt {
             getExpr :: Expr,
             getFunc :: Maybe String
           }
          | ExprStmt {
             getExpr :: Expr
           }
          | IfStmt {
             getCondition :: Expr,
             getifStmt :: Stmt,
             getelseStmt :: Maybe Stmt
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
             getStmt :: Stmt,
             getmLabel :: Maybe String
           }
          | DoWhileStmt {
             getStmt :: Stmt,
             getCondition :: Expr,
             getmLabel :: Maybe String
           }
          | ForStmt {
             getForInit :: ForInit,
             getmCondition :: Maybe Expr,
             getEnd :: Maybe Expr,
             getStmt :: Stmt,
             getmLabel :: Maybe String
           }
          | SwitchStmt {
             getExpr :: Expr,
             getStmt :: Stmt,
             getmLabel :: Maybe String,
             getmCases :: Maybe [CaseLabel]
           }
          | CaseStmt {
             getExpr :: Expr,
             getStmt :: Stmt,
             getmLabel :: Maybe String
           }
          | DefaultStmt {
             getStmt :: Stmt,
             getmLabel :: Maybe String
           }
          | NullStmt

data Expr = Binary {
             getBinOp :: BinOp,
             getLeftExpr :: Expr,
             getRightExpr :: Expr
           }
          | Assign {
             getDstExpr :: Expr,
             getSrcExpr :: Expr
           }
          | PostAssign {
             getSrc :: Expr,
             getPostOp :: PostOp
           }
          | Conditional {
             getConditionExpr :: Expr,
             getTrueExpr :: Expr,
             getFalseExpr :: Expr
           }
          | Lit Const_
          | Unary UnaryOp Expr
          | Var String
          | FunctionCall String [Expr]
          | Cast Type_ Expr
          | AddrOf Expr
          | Dereference Expr
          | Subscript Expr Expr
          deriving (Eq)

data Const_ = ConstInt {getConstInt :: Int}
            | ConstUInt {getConstInt :: Int}
            | ConstLong {getConstInt :: Int}
            | ConstULong {getConstInt :: Int}
  deriving (Show, Eq)

data CaseLabel = IntCase Int | DefaultCase
  deriving (Show, Eq)

data Declaration = VarDclr VariableDclr | FunDclr FunctionDclr
  deriving (Show)

data Declarator = IdentDec String
                | PointerDec Declarator
                | FunDec [ParamInfo] Declarator
                | ArrayDec Declarator Int

data AbstractDeclarator = AbstractPointer AbstractDeclarator
                        | AbstractArray AbstractDeclarator Int
                        | AbstractBase

data ParamInfo = Param Type_ Declarator

data StorageClass = Static | Extern
  deriving (Show, Eq)

data TypeSpecifier = IntSpec | UIntSpec | SIntSpec | LongSpec
  deriving (Show, Eq)

data DclrPrefix = TypePrefix TypeSpecifier | StoragePrefix StorageClass

data FunctionDclr = FunctionDclr {
  fName :: String,
  fType :: Type_,
  fStorage :: Maybe StorageClass,
  fParams :: [VariableDclr],
  fBody :: Maybe Block
}

data VariableDclr = VariableDclr {
  vName :: String,
  vType :: Type_,
  vStorage :: Maybe StorageClass,
  vInit :: Maybe VarInit
}

data Type_ = IntType
           | LongType
           | UIntType
           | ULongType
           | FunType {
              getParamTypes :: [Type_],
              getRetType :: Type_
            }
           | PointerType {
              getRefType :: Type_
           }
           | ArrayType {
              getItemType :: Type_,
              getSize :: Int
           }
  deriving (Eq)

data ForInit = InitDclr VariableDclr | InitExpr (Maybe Expr)
  deriving (Show)

data UnaryOp = Complement
             | Negate
             | BoolNot
             deriving (Show, Eq)

data BinOp = SubOp | AddOp | MulOp | DivOp | ModOp |
             BitAnd | BitOr  | BitXor | BitShr | BitShl |
             BoolAnd | BoolOr | BoolEq | BoolNeq |
             BoolLe | BoolGe | BoolLeq | BoolGeq |
             AssignOp | PlusEqOp | MinusEqOp | TimesEqOp |
             DivEqOp | ModEqOp | AndEqOp | OrEqOp | XorEqOp |
             ShlEqOp | ShrEqOp | TernaryOp
            deriving (Show, Eq)

data PostOp = PostInc | PostDec
  deriving (Eq, Show)

data VarInit = SingleInit Expr
             | CompoundInit [VarInit]

compoundOps :: [BinOp]
compoundOps = [PlusEqOp, MinusEqOp, TimesEqOp, DivEqOp, ModEqOp,
               AndEqOp, OrEqOp, XorEqOp, ShlEqOp, ShrEqOp]

relationalOps :: [BinOp]
relationalOps = [BoolEq, BoolNeq, BoolGe, BoolGeq, BoolLe, BoolLeq]


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

showVar :: Int -> VariableDclr -> String
showVar n (VariableDclr v type_ storageClass (Just varInit)) =
  tabs ++ "VariableDclr(\n" ++ tabs ++ "    name=\"" ++ v ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
  tabs ++ "    storage class=" ++ show storageClass ++ "\n" ++
  tabs ++ "    init=" ++ show varInit ++ "\n" ++
  tabs ++ ")"
  where tabs = replicate (4 * n) ' '
showVar n (VariableDclr v type_ storageClass Nothing) =
  tabs ++ "VariableDclr(\n" ++ tabs ++ "    name=\"" ++ v ++ "\",\n" ++
  tabs ++ "    type=" ++ show type_ ++ "\n" ++
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

instance Show FunctionDclr where
  show = showFunc 0

instance Show VariableDclr where
  show = showVar 0

instance Show Type_ where
  show IntType = "Int"
  show UIntType = "UInt"
  show LongType = "Long"
  show ULongType = "ULong"
  show (FunType paramTypes retType) =
    "Fun: param types = " ++ show paramTypes ++ ", return type = " ++ show retType
  show (PointerType refType) =
    show refType ++ "*"
  show (ArrayType itemType size) =
    "Array(" ++ show itemType ++ "," ++ show size ++ ")"

instance Show BlockItem where
  show = showBlockItem 1

showBlockItem :: Int -> BlockItem -> String
showBlockItem n (StmtBlock x) = showStatement (n + 1) x ++ ","
showBlockItem n (DclrBlock x) = showDeclaration (n + 1) x ++ ","

showDeclaration :: Int -> Declaration -> String
showDeclaration n (FunDclr f) =
  showFunc n f
showDeclaration n (VarDclr v) =
  showVar n v

showStatement :: Int -> Stmt -> String
showStatement n (RetStmt expr func) =
  tabs ++ "Return(" ++ show expr ++ ", " ++ show func ++ ")"
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

instance Show Stmt where
  show = showStatement 0

instance Show Expr where
  show (Binary op left right) =
    show op ++ "("  ++ show left ++ ", " ++ show right ++ ")"
  show (Assign x y) =
    "Assign(" ++ show x ++ ", " ++ show y ++ ")"
  show (PostAssign x op) =
    "PostAssign(" ++ show x ++ ", "++ show op ++ ")"
  show (Conditional c x y) =
    "Conditional(" ++ show c ++ ", " ++ show x ++ ", " ++ show y ++ ")"
  show (Lit n) = show n
  show (Unary op expr) = show op ++ "(" ++ show expr ++ ")"
  show (Var x) = "Var(" ++ show x ++ ")"
  show (FunctionCall name args) =
    "FunctionCall(" ++ name ++ ", " ++ show args ++ ")"
  show (Cast type_ expr) =
    "Cast(" ++ show expr ++ ", type=" ++ show type_ ++ ")"
  show (AddrOf expr) =
    "AddrOf(" ++ show expr ++ ")"
  show (Dereference expr) =
    "Dereference(" ++ show expr ++ ")"
  show (Subscript left right) =
    "Subscript(" ++ show left ++ ", " ++ show right ++ ")"

instance Show VarInit where
  show (SingleInit expr) = show expr
  show (CompoundInit inits) =
    "{" ++ intercalate "," (show <$> inits) ++ "}"