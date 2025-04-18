module TACAST where

import AST (BinOp, UnaryOp, Const_(..))
import TypedAST (Type_, SymbolTable, StaticInit)
import Utils

newtype Prog  = Prog [TopLevel] deriving (Show)
data TopLevel  = Func {
                      getName :: String,
                      getGlobal :: Bool,
                      getParams :: [String],
                      getBody :: [Instr]
                    }
                  | StaticVar {
                      getName :: String,
                      getGlobal :: Bool,
                      getType :: Type_,
                      getInit :: [StaticInit]
                    }
                  | Comment String

data Instr = Return Val
           | Unary UnaryOp Val Val -- op dst src
           | Binary BinOp Val Val Val Type_-- op dst scr1 scr2
           | CondJump Condition String
           | Cmp Val Val
           | Jump String
           | Label String
           | Copy Val Val
           | Call String Val [Val]
           | GetAddress Val Val
           | Load Val Val -- dst src_ptr
           | Store Val Val -- dst_ptr src
           | CopyToOffset Val Val Int -- dst src offset
           deriving (Show)

-- temporary variable type
data Val   = Constant {getConst :: Const_}
           | Var {getVarName :: String}

data ExprResult = PlainOperand {getVal :: Val} 
                | DereferencedPointer {getVal :: Val}
  deriving (Show)

data Condition = CondE | CondNE | CondG | CondGE | CondL | CondLE
               | CondA | CondAE | CondB | CondBE
  deriving(Show)

data TACData = TACData {
  getSymbols :: SymbolTable,
  getN :: Int
}

type TACState = State TACData

instance Show TopLevel where
  show (Func name global _ instrs) =
    "\n    Func " ++ show name ++ ", global=" ++ show global ++
    ":\n" ++ unlines (("        " ++) . show <$> instrs)
  show (StaticVar name global type_ init_) =
    "\n    StaticVar " ++ show name ++ " = " ++ show init_ ++ ", global=" ++
      show global ++ ", " ++ show type_
  show (Comment s) = "\n    Comment " ++ show s

instance Show Val where
  show (Var v) = v
  show (Constant c) = show $ getConstInt c