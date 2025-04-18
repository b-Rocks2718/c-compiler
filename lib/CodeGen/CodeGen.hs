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
import TypedAST (StaticInit(getStaticInit, ZeroInit))

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
                  | Bz   Imm -- branch if zero
                  | Bp   Imm -- branch if positive
                  | Bn   Imm -- branch if negative
                  | Bc   Imm -- branch if carry
                  | Bo   Imm -- branch if overflow
                  | Bnz  Imm -- branch if nonzero
                  | Jmp  Imm -- unconditional jump
                  | Bnc  Imm -- branch if not carry
                  | Bg   Imm -- branch if greater (signed)
                  | Bge  Imm -- branch if greater or equal (signed)
                  | Bl   Imm -- branch if less (signed)
                  | Ble  Imm -- branch if less or equal (signed)
                  | Ba   Imm -- branch if above (unsigned)
                  | Bae  Imm -- branch if above or equal (unsigned)
                  | Bb   Imm -- branch if below (unsigned)
                  | Bbe  Imm -- branch if below or equal (unsigned)
                  | Clf -- clear flags
                  | Nop
                  | Sys Exception -- calls the OS
                  | Label String
                  | Fill Int -- inserts data
                  | Space Int -- inserts 0s
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
    AsmAST.Mov (Memory addr n) (Reg r) -> [Sw r addr n]
    AsmAST.Mov (Data v) (Reg r) -> case r of
      R3 -> [Movi R4 (ImmLabel v), Sw r R3 0]
      _ -> [Movi R3 (ImmLabel v), Sw r R3 0]
    AsmAST.Mov (Reg r) (Memory addr n) -> [Lw r addr n]
    AsmAST.Mov (Reg r) (Data v) -> [Movi R3 (ImmLabel v), Lw r R3 0]
    AsmAST.Push (Reg r) -> [Push r]
    AsmAST.GetAddress (Memory r n) (Memory r' m) -> [Addi R3 r' m, Sw R3 r n]
    AsmAST.GetAddress (Memory r n) (Data s) -> [Movi R3 (ImmLabel s), Sw R3 r n]
    _ -> loads ++ operation ++ stores
    where loads = case getSrcs instr of
            [a, b] -> loada ++ loadb
              where loada = case a of
                      Reg R3 -> []
                      Memory addr n -> [Lw R3 addr n]
                      Lit n -> [Movi R3 (ImmLit n)]
                      Data v -> [Movi R3 (ImmLabel v), Lw R3 R3 0]
                      _ -> error $ "invalid source " ++ show instr
                    loadb = case b of
                      Reg R4 -> []
                      Memory addr n -> [Lw R4 addr n]
                      Lit n -> [Movi R4 (ImmLit n)]
                      Data v -> [Movi R4 (ImmLabel v), Lw R4 R4 0]
                      _ -> error $ "invalid source " ++ show instr
            [a] -> case a of
              Reg R3 -> []
              Memory addr n -> [Lw R3 addr n]
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
            AsmAST.CondJump CondE s -> [Bz (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondNE s -> [Bnz (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondG s -> [Bg (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondGE s -> [Bge (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondL s -> [Bl (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondLE s -> [Ble (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondA s -> [Ba (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondAE s -> [Bae (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondB s -> [Bb (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
            AsmAST.CondJump CondBE s -> [Bbe (ImmLit 1), Jmp (ImmLit 3), Movi R3 (ImmLabel s), Jalr R0 R3]
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
              Memory addr n -> [Sw R3 addr n]
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
topLevelToMachine (AsmAST.StaticVar name _ n) =
  Label name : (makeData <$> n)
topLevelToMachine (AsmAST.Comment s) = [NlComment s]

makeData :: StaticInit -> MachineInstr
makeData (ZeroInit n) = Space n
makeData other = Fill . getStaticInit $ other

progToMachine :: AsmAST.Prog -> [MachineInstr]
progToMachine (AsmAST.Prog topLevels) =
  [Movi R3 (ImmLabel "main"), Jalr R0 R3] ++ (topLevels >>= topLevelToMachine)

asmToStr :: MachineInstr -> String
asmToStr (Label s) = s ++ ":"
asmToStr (Call s) = "\tcall " ++ s
asmToStr (Sys exc) = "\tsys " ++ (toUpper <$> show exc)
asmToStr (Jmp s) = "\tjmp " ++ show s
asmToStr (Bz s) = "\tbz " ++ show s
asmToStr (Bp s) = "\tbp " ++ show s
asmToStr (Bn s) = "\tbn " ++ show s
asmToStr (Bc s) = "\tbc " ++ show s
asmToStr (Bo s) = "\tbo " ++ show s
asmToStr (Bnz s) = "\tbnz " ++ show s
asmToStr (Bnc s) = "\tbnc " ++ show s
asmToStr (Bg s) = "\tbg " ++ show s
asmToStr (Bge s) = "\tbge " ++ show s
asmToStr (Bl s) = "\tbl " ++ show s
asmToStr (Ble s) = "\tble " ++ show s
asmToStr (Ba s) = "\tba " ++ show s
asmToStr (Bae s) = "\tbae " ++ show s
asmToStr (Bb s) = "\tbb " ++ show s
asmToStr (Bbe s) = "\tbbe " ++ show s
asmToStr (Fill s) = "\t.fill " ++ show s
asmToStr (Space s) = "\t.space " ++ show s
asmToStr (NlComment s) = "\n# " ++ s
asmToStr (Comment s) = "\t# " ++ s
asmToStr (Movi r imm) = "\tmovi " ++ (toLower <$> show r) ++ show imm
asmToStr instr =
  '\t' : filter (\c -> c/= '(' && c /= ')') (toLower <$> show instr)