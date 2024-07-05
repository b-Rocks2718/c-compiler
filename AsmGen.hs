
module AsmGen where

import Lexer
import Parser
import System.Environment

data AsmProg = AsmProg AsmFunc deriving (Show)
data AsmFunc = AsmFunc String [AsmInstr] deriving (Show)

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7

instance Show Reg where
  show R0 = "r0 "
  show R1 = "r1 "
  show R2 = "r2 "
  show R3 = "r3 "
  show R4 = "r4 "
  show R5 = "r5 "
  show R6 = "r6 "
  show R7 = "r7 "

data AsmInstr = Add  Reg Reg Reg
              | Addc Reg Reg Reg -- add w/ carry
              | Sub  Reg Reg Reg
              | Subc Reg Reg Reg -- subtract with carry
              | And  Reg Reg Reg
              | Or   Reg Reg Reg
              | Xor  Reg Reg Reg
              | Nand Reg Reg Reg
              | Addi Reg Reg Int
              | Sw   Reg Reg Int -- store word
              | Lw   Reg Reg Int -- load word
              | Not  Reg Reg
              | Shl  Reg Reg -- shift left
              | Shr  Reg Reg -- shift right
              | Rotl Reg Reg -- rotate left
              | Rotr Reg Reg -- rotate right
              | Sshr Reg Reg -- signed shift right
              | Shrc Reg Reg -- shift right w/ carry
              | Shlc Reg Reg -- shift left w/ carry
              | Cmp  Reg Reg -- compare
              | Lui  Reg Int -- load upper immediate
              | Jalr Reg Reg -- jump and link register
              | Bz   String -- branch if zero
              | Bp   String -- branch if positive
              | Bn   String -- branch if negative
              | Bc   String -- branch if carry
              | Bo   String -- branch if overflow
              | Bnz  String -- branch if nonzero
              | Jmp  String -- unconditional jump
              | Bnc  String -- branch if not carry
              | Clf -- clear flags
              | Nop
              | Sys Exception -- calls the OS
              | Label String

instance Show AsmInstr where
  show (Addi rA rB n) =
    "\taddi " ++ show rA ++ show rB ++ show n ++ "\n"
  show (Add rA rB rC) =
    "\tadd " ++ show rA ++ show rB ++ show rC ++ "\n"
  show (Sys exc) = "\tsys " ++ show exc ++ "\n"

data Exception = Exit -- will add more exceptions as OS is developed

instance Show Exception where
  show Exit = "EXIT"

progToAsm :: ASTProg -> AsmProg
progToAsm (ASTProg f) = AsmProg $ funcToAsm f

funcToAsm :: ASTFunc -> AsmFunc
funcToAsm (ASTFunc name body) = AsmFunc name (stmtToAsm body)

stmtToAsm :: ASTStmt -> [AsmInstr]
stmtToAsm (ASTReturn expr) = exprToAsm expr ++ [Sys Exit]

exprToAsm :: ASTExpr -> [AsmInstr]
exprToAsm (ASTLit n) = [Addi R7 R0 n]
