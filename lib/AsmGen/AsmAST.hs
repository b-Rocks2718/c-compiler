module AsmAST where

import AST(UnaryOp, BinOp)
import TypedAST(StaticInit, Type_)
import TACAST(Condition)

newtype Prog = Prog [TopLevel] deriving (Show)
data TopLevel = Func String Bool [Instr]
              | StaticVar String Bool [StaticInit] -- AsmStaticVar name global init
              | Comment String

data Instr = Mov Operand Operand
           | Unary UnaryOp Operand Operand
           | Binary BinOp Operand Operand Operand Type_
           | Cmp Operand Operand
           | AllocateStack Int -- addi r1 r1 n
           | Push Operand
           | Call String
           | Jump String
           | CondJump Condition String
           | Label String
           | Ret -- pop r3, jalr r0 r3
           | GetAddress Operand Operand
           deriving (Show)

data Operand = Lit Int
             | Reg Reg
             | Pseudo String -- pseudoregister/identifier
             | PseudoMem String Int
             | Memory Reg Int
             | Data String
             deriving (Show, Eq)

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
         deriving (Eq)
-- R0 := 0 and writes are ignored
-- R1 is stack pointer
-- R2 is base pointer
-- R3 is the function's return value
-- R7 has the return address
sp :: Reg
sp = R1

bp :: Reg
bp = R2

instance Show TopLevel where
  show (Func name global instrs) =
    "\n    Func " ++ show name ++ ", global=" ++ show global ++ 
    ":\n" ++ unlines (("        " ++) . show <$> instrs)
  show (StaticVar name global init_) =
    "\n    StaticVar " ++ show name ++ " " ++
      show global ++ " " ++ show init_
  show (Comment s) = "\n    Comment " ++ show s

instance Show Reg where
  show R0 = "r0 "
  show R1 = "r1 "
  show R2 = "r2 "
  show R3 = "r3 "
  show R4 = "r4 "
  show R5 = "r5 "
  show R6 = "r6 "
  show R7 = "r7 "