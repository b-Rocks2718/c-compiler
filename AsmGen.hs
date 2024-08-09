module AsmGen where

import Lexer
import Parser
import TAC

newtype AsmProg = AsmProg AsmFunc deriving (Show)
data AsmFunc = AsmFunc String [AsmInstr]

data AsmInstr = Mov Operand Operand
              | AsmUnary UnaryOp Operand Operand
              | AsmBinary BinOp Operand Operand Operand
              | AsmCmp Operand Operand
              | AllocateStack Int -- sub r1 r1 n
              | AsmJump String
              | AsmCondJump Condition String
              | AsmLabel String
              | Ret -- pop r3, jalr r0 r3
              deriving (Show)

data Operand = AsmLit Int
             | Reg Reg
             | Pseudo String -- pseudoregister/identifier
             | Stack Int -- location relative to sp
             deriving (Show, Eq)

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
         deriving (Eq)
-- R0 := 0 and writes are ignored
-- R1 is stack pointer
-- R2 is base pointer
-- R3 is the function's return value
sp :: Reg
sp = R1

bp :: Reg
bp = R2

instance Show AsmFunc where
  show (AsmFunc name instrs) =
    "AsmFunc " ++ show name ++ ":\n" ++
    unlines (show <$> instrs)

instance Show Reg where
  show R0 = "r0 "
  show R1 = "r1 "
  show R2 = "r2 "
  show R3 = "r3 "
  show R4 = "r4 "
  show R5 = "r5 "
  show R6 = "r6 "
  show R7 = "r7 "

progToAsm :: TACProg -> AsmProg
progToAsm (TACProg f) = AsmProg $ funcToAsm f

funcToAsm :: TACFunc -> AsmFunc
funcToAsm (TACFunc name body) =
  AsmFunc name $ replacePseudo maps <$> instrs
  where (maps, size) = createMaps instrs
        instrs = AllocateStack (size + 1) : (body >>= exprToAsm)

exprToAsm :: TACInstr -> [AsmInstr]
exprToAsm instr = 
  case instr of 
    (TACReturn val) -> [Mov (Reg R3) (tacValToAsm val), Ret]
    (TACCopy dst src) -> [Mov (tacValToAsm dst) (tacValToAsm src)]
    (TACUnary op dst src) ->
      [AsmUnary op (tacValToAsm dst) (tacValToAsm src)]
    (TACBinary op dst src1 src2) -> 
      [AsmBinary op (tacValToAsm dst) (tacValToAsm src1) (tacValToAsm src2)]
    (TACCondJump cond label) -> [AsmCondJump cond label]
    (TACCmp val1 val2) -> [AsmCmp (tacValToAsm val1) (tacValToAsm val2)]
    (TACJump label) -> [AsmJump label]
    (TACLabel s) -> [AsmLabel s]

createMaps :: [AsmInstr] -> ([(Operand, Operand)], Int)
createMaps xs = foldr f ([], -1) (xs >>= getOps)
  where f opr (maps, size) =
          case opr of
            (Pseudo _) -> case lookup opr maps of
              (Just _) -> (maps, size)
              Nothing -> ((opr, Stack size):maps, size - 1)
            _ -> (maps, size)

getOps :: AsmInstr -> [Operand]
getOps x = getDst x ++ getSrcs x

getSrcs :: AsmInstr -> [Operand]
getSrcs x = 
  case x of
    Mov a b -> [b]
    AsmUnary _ a b -> [b]
    AsmBinary _ a b c -> [b, c]
    AsmCmp a b -> [a, b]
    _ -> []

getDst :: AsmInstr -> [Operand]
getDst x =
  case x of 
    Mov a b -> [a]
    AsmUnary _ a b -> [a]
    AsmBinary _ a b c -> [a]
    _ -> []

-- map identifiers used as src/dst to stack locations
replacePseudo :: [(Operand, Operand)] -> AsmInstr -> AsmInstr
replacePseudo maps = mapOps f
  where f op = case lookup op maps of
          (Just newOp) -> newOp
          Nothing -> op

mapOps :: (Operand -> Operand) -> AsmInstr -> AsmInstr
mapOps f (Mov a b) = Mov (f a) (f b)
mapOps f (AsmUnary op a b) = AsmUnary op (f a) (f b)
mapOps f (AsmBinary op a b c) = AsmBinary op (f a) (f b) (f c)
mapOps f (AsmCmp a b) = AsmCmp (f a) (f b)
mapOps f x = x
  
tacValToAsm :: TACVal -> Operand
tacValToAsm (TACLit n) = AsmLit n
tacValToAsm (TACVar s) = Pseudo s