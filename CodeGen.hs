
module CodeGen where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Char ( toLower, toUpper )

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
                  | Movi Reg Int
                  | Jalr Reg Reg -- jump and link register
                  | Push Reg
                  | Pop  Reg
                  | Call String
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

-- R3 and R4 are used as scratch registers
-- this function assumes no other registers are used in previous stage
-- TODO: eliminate that assumtion
toMachineInstr :: AsmInstr -> [MachineInstr]
toMachineInstr instr = loads ++ operation ++ stores
  where loads = case getSrcs instr of
          [a, b] -> loada ++ loadb
            where loada = case a of
                    Reg R3 -> []
                    Stack n -> [Lw R3 sp n]
                    AsmLit n -> [Movi R3 n]
                    _ -> error "invalid source"
                  loadb = case b of
                    Reg R4 -> []
                    Stack n -> [Lw R4 sp n]
                    AsmLit n -> [Movi R4 n]
                    _ -> error "invalid source"
          [a] -> case a of
            Reg R3 -> []
            Stack n -> [Lw R3 sp n]
            AsmLit n -> [Movi R3 n]
            _ -> error "invalid source"
          _ -> []
        operation = case instr of
          Mov _ _ -> []
          AsmUnary Complement _ _ -> [Not R3 R3]
          AsmUnary Negate _ _ -> [Sub R3 R0 R3]
          AsmBinary AddOp _ _ _ -> [Add R3 R3 R4]
          AsmBinary SubOp _ _ _ -> [Sub R3 R3 R4]
          AsmBinary MulOp _ _ _ -> [Call "mul"]
          AsmBinary DivOp _ _ _ -> [Call "miv"]
          AsmBinary ModOp _ _ _ -> [Call "mod"]
          AllocateStack n -> [Addi sp sp n] -- n should be negative
          Ret -> [Addi sp bp 0, Pop bp, Pop R7] -- (+ Jalr R0 R7)
        stores = case getDst instr of
          [a] -> case a of
            Reg r -> []
            Stack n -> [Sw R3 sp n]
            _ -> error "invalid destination"
          _ -> []

progToMachine :: AsmProg -> [MachineInstr]
progToMachine (AsmProg (AsmFunc name instrs)) =
  [Label name, Push R7, Push bp, Addi bp sp 0] ++
  (instrs >>= toMachineInstr) ++
  [Sys Exit]

asmToStr :: MachineInstr -> String
asmToStr (Label s) = s ++ ":"
asmToStr (Call s) = "\tcall " ++ s
asmToStr (Sys exc) = "\tsys " ++ (toUpper <$> show exc)
asmToStr instr =
  '\t' : filter (\c -> c/= '(' && c /= ')') (toLower <$> show instr)

writeEither :: String -> Either a String -> IO ()
writeEither path (Right s) = writeFile path s
writeEither _ (Left _) = return ()

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  content <- readFile path
  --putStrLn ("Input program:\n" ++ content)
  let processed = preprocess content
  --putStrLn ("Preprocessed code:\n" ++ processed)
  let tokens = lexerEval processed
  --putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = tokens >>= (runParser parseProgram . fst)
  --putStrLn ("\nSyntax tree:\n" ++ showAST ast)
  let tac = progToTAC . fst <$> ast
  --putStrLn ("\nTAC:\n" ++ showEither tac)
  let asm = progToAsm <$> tac
  --putStrLn ("AsmAST:\n" ++ showEither asm)
  let asm' = progToMachine <$> asm
  let code = unlines . fmap asmToStr <$> asm'
  --putStrLn ("Code:\n" ++ showEitherStr code)
  let fileName = head $ splitOn "." path
  let asmFile = fileName ++ ".s"
  writeEither asmFile code
