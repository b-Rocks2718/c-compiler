module CodeGen where

import Utils
import qualified AsmAST
import AST(UnaryOp(..), BinOp(..), Type_ (..))
import TACAST(Condition(..))
import AsmAST (
  Reg (..),
  Operand(..),
  sp,
  bp)
import AsmGen(getSrcs, getDst)
import TypedAST (StaticInit(getStaticInit))

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
                  | Bg   String -- branch if greater (signed)
                  | Bge  String -- branch if greater or equal (signed)
                  | Bl   String -- branch if less (signed)
                  | Ble  String -- branch if less or equal (signed)
                  | Ba   String -- branch if above (unsigned)
                  | Bae  String -- branch if above or equal (unsigned)
                  | Bb   String -- branch if below (unsigned)
                  | Bbe  String -- branch if below or equal (unsigned)
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
toMachineInstr :: String -> AsmAST.Instr -> [MachineInstr]
toMachineInstr name instr = 
  case instr of
    -- kind of a hack but it seems to work for now
    AsmAST.Mov (Reg r1) (Reg r2) -> [Mov r1 r2]
    AsmAST.Mov (Reg r) (Lit n) -> [Movi r (ImmLit n)]
    AsmAST.Mov (Stack n) (Reg r) -> [Sw r bp n]
    AsmAST.Mov (Data v) (Reg r) -> case r of
      R3 -> [Movi R4 (ImmLabel v), Sw r R3 0]
      _ -> [Movi R3 (ImmLabel v), Sw r R3 0]
    AsmAST.Mov (Reg r) (Stack n) -> [Lw r bp n]
    AsmAST.Mov (Reg r) (Data v) -> [Movi R3 (ImmLabel v), Lw r R3 0]
    AsmAST.Push (Reg r) -> [Push r]
    _ -> loads ++ operation ++ stores
    where loads = case getSrcs instr of
            [a, b] -> loada ++ loadb
              where loada = case a of
                      Reg R3 -> []
                      Stack n -> [Lw R3 bp n]
                      Lit n -> [Movi R3 (ImmLit n)]
                      Data v -> [Movi R3 (ImmLabel v), Lw R3 R3 0]
                      _ -> error $ "invalid source " ++ show instr
                    loadb = case b of
                      Reg R4 -> []
                      Stack n -> [Lw R4 bp n]
                      Lit n -> [Movi R4 (ImmLit n)]
                      Data v -> [Movi R4 (ImmLabel v), Lw R4 R4 0]
                      _ -> error $ "invalid source " ++ show instr
            [a] -> case a of
              Reg R3 -> []
              Stack n -> [Lw R3 bp n]
              Lit n -> [Movi R3 (ImmLit n)]
              Data v -> [Movi R3 (ImmLabel v), Lw R3 R3 0]
              _ -> error $ "invalid source " ++ show instr
            _ -> []
          operation = case instr of
            AsmAST.Mov _ _ -> []
            AsmAST.Cmp _ _ -> [Cmp R3 R4]
            AsmAST.Unary Complement _ _ -> [Not R3 R3]
            AsmAST.Unary Negate _ _ -> [Sub R3 R0 R3]
            AsmAST.Binary AddOp _ _ _ _ -> [Add R3 R3 R4]
            AsmAST.Binary SubOp _ _ _ _ -> [Sub R3 R3 R4]
            AsmAST.Binary BitAnd _ _ _ _ -> [And R3 R3 R4]
            AsmAST.Binary BitOr _ _ _ _ -> [Or R3 R3 R4]
            AsmAST.Binary BitXor _ _ _ _ -> [Xor R3 R3 R4]
            AsmAST.Binary MulOp _ _ _ IntType -> [Call "smul"]
            AsmAST.Binary DivOp _ _ _ IntType -> [Call "sdiv"]
            AsmAST.Binary ModOp _ _ _ IntType -> [Call "smod"]
            AsmAST.Binary MulOp _ _ _ UIntType -> [Call "umul"]
            AsmAST.Binary DivOp _ _ _ UIntType -> [Call "udiv"]
            AsmAST.Binary ModOp _ _ _ UIntType -> [Call "umod"]
            AsmAST.Binary BitShl _ _ _ _ -> [Call "left_shift"]
            AsmAST.Binary BitShr _ _ _ _ -> [Call "right_shift"]
            AsmAST.Jump s -> [Movi R3 (ImmLabel s), Jalr R0 R3] -- will optimize later
            AsmAST.CondJump CondE s -> [Bz s]
            AsmAST.CondJump CondNE s -> [Bnz s]
            AsmAST.CondJump CondG s -> [Bg s]
            AsmAST.CondJump CondGE s -> [Bge s]
            AsmAST.CondJump CondL s -> [Bl s]
            AsmAST.CondJump CondLE s -> [Ble s]
            AsmAST.CondJump CondA s -> [Ba s]
            AsmAST.CondJump CondAE s -> [Bae s]
            AsmAST.CondJump CondB s -> [Bb s]
            AsmAST.CondJump CondBE s -> [Bbe s]
            AsmAST.Label s -> [Label s]
            AsmAST.AllocateStack n -> if n == 0 then []
              else if n >= -64 && n <= 63
                then [Addi sp sp n]
              else [Movi R3 (ImmLit n), Add sp sp R3]
            AsmAST.Call f -> [Call f]
            AsmAST.Push _ -> [Push R3]
            AsmAST.Ret -> case name of 
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
topLevelToMachine :: AsmAST.TopLevel -> [MachineInstr]
topLevelToMachine (AsmAST.Func name _ instrs) = 
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
topLevelToMachine (AsmAST.StaticVar name _ n) = [Label name, Fill (getStaticInit n)]
topLevelToMachine (AsmAST.Comment s) = [NlComment s]

progToMachine :: AsmAST.Prog -> [MachineInstr]
progToMachine (AsmAST.Prog topLevels) = 
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
asmToStr (Ba s) = "\tba " ++ s
asmToStr (Bae s) = "\tbae " ++ s
asmToStr (Bb s) = "\tbb " ++ s
asmToStr (Bbe s) = "\tbbe " ++ s
asmToStr (Fill s) = "\t.fill " ++ show s
asmToStr (NlComment s) = "\n# " ++ s
asmToStr (Comment s) = "\t# " ++ s
asmToStr (Movi r imm) = "\tmovi " ++ (toLower <$> show r) ++ show imm
asmToStr instr =
  '\t' : filter (\c -> c/= '(' && c /= ')') (toLower <$> show instr)