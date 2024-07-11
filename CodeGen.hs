
module CodeGen where

import System.Environment
import Data.List.Split
import Data.Char

import Lexer
import Parser
import TAC
import AsmGen

data MachineInstr = Add  Reg Reg Reg
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
                  | Push Reg
                  | Pop  Reg
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
                  deriving (Show)

-- will add more exceptions as OS is developed
data Exception = Exit
               deriving (Show)

toMachineInstr :: AsmInstr -> [MachineInstr]
toMachineInstr (Mov (Stack n) (AsmLit x)) = [Addi R3 R0 x, Sw R3 sp n]
toMachineInstr (Mov (Stack m) (Stack n)) = [Lw R3 sp n, Sw R3 sp m]
toMachineInstr (Mov (Reg r) (Stack n)) = [Lw r sp n]
toMachineInstr (Mov (Reg r) (AsmLit x)) = [Addi r R0 x]
toMachineInstr (AsmUnary Complement (Stack n)) =
  [Lw R3 sp n, Not R3 R3, Sw R3 sp n]
toMachineInstr (AsmUnary Negate (Stack n)) =
  [Lw R3 sp n, Sub R3 R0 R3, Sw R3 sp n]
toMachineInstr (AsmUnary Complement (Reg r)) = [Not r r]
toMachineInstr (AsmUnary Negate (Reg r)) = [Sub r R0 r]
toMachineInstr (AllocateStack n) = [Addi sp sp n] -- n should be negative
toMachineInstr (Ret) = [Addi sp bp 0, Pop bp, Pop R7]
                       -- Jalr R0 R7 for non-main functions

progToMachine :: AsmProg -> [MachineInstr]
progToMachine (AsmProg (AsmFunc name instrs)) =
  [Label name, Push R7, Push bp, Addi bp sp 0] ++
  (instrs >>= toMachineInstr) ++
  [Sys Exit]

asmToStr :: MachineInstr -> String
asmToStr (Label s) = s ++ ":"
asmToStr (Sys exc) = "\tsys " ++ (toUpper <$> show exc)
asmToStr instr =
  '\t' : filter (\c -> c/= '(' && c /= ')') (toLower <$> (show instr))

writeEither :: String -> Either a String -> IO ()
writeEither path (Right s) = writeFile path s
writeEither _ (Left _) = return ()

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  content <- readFile path
  putStrLn ("Input program:\n" ++ content)
  let processed = preprocess content
  putStrLn ("Preprocessed code:\n" ++ processed)
  let tokens = lexerEval processed
  putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = (fst <$> tokens) >>= (runParser parseProgram)
  putStrLn ("\nSyntax tree:\n" ++ showAST ast)
  let tac = progToTAC <$> fst <$> ast
  putStrLn ("\nTAC:\n" ++ showEither tac)
  let asm = progToAsm <$> tac
  putStrLn ("AsmAST:\n" ++ showEither asm)
  let asm' = progToMachine <$> asm
  let code = unlines <$> (fmap asmToStr <$> asm')
  putStrLn ("Code:\n" ++ showEitherStr code)
  let fileName = (head $ splitOn "." path)
  let asmFile = fileName ++ ".s"
  writeEither asmFile code
