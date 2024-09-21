module CodeGen where

import System.Environment ( getArgs )
import Data.List.Split ( splitOn )
import Data.Char ( toLower, toUpper )
import Control.Monad (when)
import Control.Applicative

import Lexer
import Parser
import Semantics
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
                  | Mov  Reg Reg
                  | Lui  Reg Int -- load upper immediate
                  | Movi Reg Imm
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
                  | Bg   String -- branch if greater
                  | Bge  String -- branch if greater or equal
                  | Bl   String -- branch if less
                  | Ble  String -- branch if less or equal
                  | Clf -- clear flags
                  | Nop
                  | Sys Exception -- calls the OS
                  | Label String
                  | Fill Int -- inserts data
                  | NlComment String
                  | Comment String
                  deriving (Show)

data Imm = ImmLit Int | ImmLabel String

instance Show Imm where
  show (ImmLit n) = show n
  show (ImmLabel s) = s

-- will add more exceptions as OS is developed
data Exception = Exit
  deriving (Show)

-- R3 and R4 are used as scratch registers
-- this function assumes no other registers are used in previous stage
-- hopefully I can fix this nonsense when I write a register allocator
toMachineInstr :: String -> AsmInstr -> [MachineInstr]
toMachineInstr name instr = 
  case instr of
    -- kind of a hack but it seems to work for now
    AsmMov (Reg r1) (Reg r2) -> [Mov r1 r2]
    AsmMov (Reg r) (AsmLit n) -> [Movi r (ImmLit n)]
    AsmMov (Stack n) (Reg r) -> [Sw r bp n]
    AsmMov (Data v) (Reg r) -> case r of
      R3 -> [Movi R4 (ImmLabel v), Sw r R3 0]
      _ -> [Movi R3 (ImmLabel v), Sw r R3 0]
    AsmMov (Reg r) (Stack n) -> [Lw r bp n]
    AsmMov (Reg r) (Data v) -> [Movi R3 (ImmLabel v), Lw r R3 0]
    AsmPush (Reg r) -> [Push r]
    _ -> loads ++ operation ++ stores
    where loads = case getSrcs instr of
            [a, b] -> loada ++ loadb
              where loada = case a of
                      Reg R3 -> []
                      Stack n -> [Lw R3 bp n]
                      AsmLit n -> [Movi R3 (ImmLit n)]
                      Data v -> [Movi R3 (ImmLabel v), Lw R3 R3 0]
                      _ -> error $ "invalid source " ++ show instr
                    loadb = case b of
                      Reg R4 -> []
                      Stack n -> [Lw R4 bp n]
                      AsmLit n -> [Movi R4 (ImmLit n)]
                      Data v -> [Movi R4 (ImmLabel v), Lw R4 R4 0]
                      _ -> error $ "invalid source " ++ show instr
            [a] -> case a of
              Reg R3 -> []
              Stack n -> [Lw R3 bp n]
              AsmLit n -> [Movi R3 (ImmLit n)]
              Data v -> [Movi R3 (ImmLabel v), Lw R3 R3 0]
              _ -> error $ "invalid source " ++ show instr
            _ -> []
          operation = case instr of
            AsmMov _ _ -> []
            AsmCmp _ _ -> [Cmp R3 R4]
            AsmUnary Complement _ _ -> [Not R3 R3]
            AsmUnary Negate _ _ -> [Sub R3 R0 R3]
            AsmBinary AddOp _ _ _ -> [Add R3 R3 R4]
            AsmBinary SubOp _ _ _ -> [Sub R3 R3 R4]
            AsmBinary BitAnd _ _ _ -> [And R3 R3 R4]
            AsmBinary BitOr _ _ _ -> [Or R3 R3 R4]
            AsmBinary BitXor _ _ _ -> [Xor R3 R3 R4]
            AsmBinary MulOp _ _ _ -> [Call "mul"]
            AsmBinary DivOp _ _ _ -> [Call "div"]
            AsmBinary ModOp _ _ _ -> [Call "mod"]
            AsmBinary BitShl _ _ _ -> [Call "left_shift"]
            AsmBinary BitShr _ _ _ -> [Call "right_shift"]
            AsmJump s -> [Movi R3 (ImmLabel s), Jalr R0 R3] -- will optimize later
            AsmCondJump CondE s -> [Bz s]
            AsmCondJump CondNE s -> [Bnz s]
            AsmCondJump CondG s -> [Bg s]
            AsmCondJump CondGE s -> [Bge s]
            AsmCondJump CondL s -> [Bl s]
            AsmCondJump CondLE s -> [Ble s]
            AsmLabel s -> [Label s]
            AllocateStack n -> if n == 0 then []
              else if n >= -64 && n <= 63
                then [Addi sp sp n]
              else [Movi R3 (ImmLit n), Add sp sp R3]
            AsmCall f -> [Call f]
            AsmPush _ -> [Push R3]
            Ret -> case name of 
              -- function epilogues
                    "main" -> [Comment "Function Epilogue", Sys Exit]
                    _ -> [Comment "Function Epilogue",
                          Mov sp bp,
                          Lw R7 bp 1,
                          Lw bp bp 0,
                          Addi sp sp 2,
                          Jalr R0 R7]
            x -> error $ "Compiler Error: undefined operation " ++ show x
          stores = case getDst instr of
            [a] -> case a of
              Reg R3 -> []
              Stack n -> [Sw R3 bp n]
              Data v -> [Movi R4 (ImmLabel v), Sw R3 R4 0]
              _ -> error $ "invalid destination "  ++ show instr
            _ -> []

-- global will be ignored until I update the assembler
topLevelToMachine :: AsmTopLevel -> [MachineInstr]
topLevelToMachine (AsmFunc name global instrs) = 
  case name of
    -- function prologues
    "main" -> [Label name, 
               Addi sp R0 0,
               Addi bp R0 0]
    _ -> [Label name,
          Comment "Function Prologue", 
          Sw R7 sp (-1), 
          Sw bp sp (-2), 
          Addi sp sp (-2),
          Addi bp sp 0,
          Comment "Function Body"] 
  ++ (instrs >>= toMachineInstr name)
topLevelToMachine (AsmStaticVar name global n) = [Label name, Fill n]
topLevelToMachine (AsmComment s) = [NlComment s]

progToMachine :: AsmProg -> [MachineInstr]
progToMachine (AsmProg topLevels) = 
  [Movi R3 (ImmLabel "main"), Jalr R0 R3] ++ (topLevels >>= topLevelToMachine)

asmToStr :: MachineInstr -> String
asmToStr (Label s) = s ++ ":"
asmToStr (Call s) = "\tcall " ++ s
asmToStr (Sys exc) = "\tsys " ++ (toUpper <$> show exc)
asmToStr (Jmp s) = "\tjmp " ++ s
asmToStr (Bz s) = "\tbz " ++ s
asmToStr (Bp s) = "\tbp " ++ s
asmToStr (Bn s) = "\tbn " ++ s
asmToStr (Bc s) = "\tbc " ++ s
asmToStr (Bo s) = "\tbo " ++ s
asmToStr (Bnz s) = "\tbnz " ++ s
asmToStr (Bnc s) = "\tbnc " ++ s
asmToStr (Bg s) = "\tbg " ++ s
asmToStr (Bge s) = "\tbge " ++ s
asmToStr (Bl s) = "\tbl " ++ s
asmToStr (Ble s) = "\tble " ++ s
asmToStr (Fill s) = "\t.fill " ++ show s
asmToStr (NlComment s) = "\n# " ++ s
asmToStr (Comment s) = "\t# " ++ s
asmToStr instr =
  '\t' : filter (\c -> c/= '(' && c /= ')') (toLower <$> show instr)

writeEither :: String -> Either a String -> IO ()
writeEither path (Right s) = writeFile path s
writeEither _ (Left _) = return ()

-- to display errors
showLeft :: Either String a -> IO ()
showLeft (Right _) = return ()
showLeft (Left s) = putStrLn s

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  let flags = tail args
  content <- readFile path
  let processed = preprocess content
  let tokens = lexerEval processed
  when ("--tokens" `elem` flags) $ do
    putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = tokens >>= (runParser parseProgram . fst)
  when ("--ast" `elem` flags) $ do
    putStrLn ("\nSyntax tree:\n" ++ showEither (fst <$> ast))
  let resolved = ast >>= resolveProg . fst
  let symbols = fst <$> resolved
  when ("--semantics" `elem` flags) $ do
    putStrLn ("\nResolved tree:\n" ++ showEither (snd <$> resolved))
    putStrLn ("\nSymbol Table:\n" ++ showSymbols symbols)
  let tac = uncurry progToTAC <$> resolved
  when ("--tac" `elem` flags) $ do
    putStrLn ("\nTAC:\n" ++ showEither tac)
  let asm = liftA2 progToAsm tac symbols
  when ("--asm" `elem` flags) $ do
    putStrLn ("\nAsmAST:\n" ++ showEither asm)
  let asm' = progToMachine <$> asm
  let code = unlines . fmap asmToStr <$> asm'
  when ("--code" `elem` flags) $ do
    putStrLn ("Code:\n" ++ showEitherStr code)
  showLeft code
  let fileName = head $ splitOn "." path
  let asmFile = fileName ++ ".s"
  writeEither asmFile code
