module AsmGen where

import Lexer
import Parser
import TAC
import Semantics
import Control.Applicative

newtype AsmProg = AsmProg [AsmTopLevel] deriving (Show)
data AsmTopLevel = AsmFunc String Bool [AsmInstr]
                 | AsmStaticVar String Bool Int -- AsmStaticVar name global init
                 | AsmComment String

data AsmInstr = AsmMov Operand Operand
              | AsmUnary UnaryOp Operand Operand
              | AsmBinary BinOp Operand Operand Operand
              | AsmCmp Operand Operand
              | AllocateStack Int -- addi r1 r1 n
              | AsmPush Operand
              | AsmCall String
              | AsmJump String
              | AsmCondJump Condition String
              | AsmLabel String
              | Ret -- pop r3, jalr r0 r3
              deriving (Show)

data Operand = AsmLit Int
             | Reg Reg
             | Pseudo String -- pseudoregister/identifier
             | Stack Int -- location relative to sp
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

instance Show AsmTopLevel where
  show (AsmFunc name global instrs) =
    "\n    AsmFunc " ++ show name ++ ", global=" ++ show global ++ 
    ":\n" ++ unlines (("        " ++) . show <$> instrs)
  show (AsmStaticVar name global init) =
    "\n    AsmStaticVar " ++ show name ++ " " ++
      show global ++ " " ++ show init
  show (AsmComment s) = "\n    TACComment " ++ show s

instance Show Reg where
  show R0 = "r0 "
  show R1 = "r1 "
  show R2 = "r2 "
  show R3 = "r3 "
  show R4 = "r4 "
  show R5 = "r5 "
  show R6 = "r6 "
  show R7 = "r7 "

progToAsm :: TACProg -> SymbolTable -> AsmProg
progToAsm (TACProg p) symbols = AsmProg (topLevelToAsm symbols <$> p)

topLevelToAsm :: SymbolTable -> TACTopLevel -> AsmTopLevel
topLevelToAsm symbols (TACFunc name global params body) =
  AsmFunc name global $ replacePseudo maps <$> instrs
  where (maps, size) = createMaps instrs symbols
        instrs = AllocateStack (size + 1)  : paramsToAsm params ++ (body >>= exprToAsm)
topLevelToAsm _ (TACStaticVar name global init) = AsmStaticVar name global init
topLevelToAsm _ (TACComment s) = AsmComment s

paramsToAsm :: [String] -> [AsmInstr]
paramsToAsm ps = getZipList (ZipList copyList <*> ZipList ps)
  where copyList =
          [\s -> AsmMov (Pseudo s) (Reg R3),
           \s -> AsmMov (Pseudo s) (Reg R4),
           \s -> AsmMov (Pseudo s) (Reg R5),
           \s -> AsmMov (Pseudo s) (Reg R6)] ++
           ((\n s -> AsmMov (Pseudo s) (Stack n)) <$> [2..])

exprToAsm :: TACInstr -> [AsmInstr]
exprToAsm instr =
  case instr of
    (TACReturn val) -> [AsmMov (Reg R3) (tacValToAsm val), Ret]
    (TACCopy dst src) -> [AsmMov (tacValToAsm dst) (tacValToAsm src)]
    (TACUnary op dst src) ->
      [AsmUnary op (tacValToAsm dst) (tacValToAsm src)]
    (TACBinary op dst src1 src2) ->
      [AsmBinary op (tacValToAsm dst) (tacValToAsm src1) (tacValToAsm src2)]
    (TACCondJump cond label) -> [AsmCondJump cond label]
    (TACCmp val1 val2) -> [AsmCmp (tacValToAsm val1) (tacValToAsm val2)]
    (TACJump label) -> [AsmJump label]
    (TACLabel s) -> [AsmLabel s]
    (TACCall name dst srcs) -> 
      -- push in reverse order
      getZipList (ZipList stackList <*> ZipList (reverse stackArgs)) ++
      getZipList (ZipList regList <*> ZipList regArgs) ++
      [AsmCall name, 
      AllocateStack (length stackArgs), -- deallocate stack
      AsmMov (tacValToAsm dst) (Reg R3)]
      where regList =
              [AsmMov (Reg R3) . tacValToAsm,
               AsmMov (Reg R4) . tacValToAsm,
               AsmMov (Reg R5) . tacValToAsm,
               AsmMov (Reg R6) . tacValToAsm]
            regArgs = take 4 srcs
            stackArgs = drop 4 srcs
            stackList = repeat (AsmPush . tacValToAsm)

createMaps :: [AsmInstr] -> SymbolTable -> ([(Operand, Operand)], Int)
createMaps xs symbols = foldr (createMapsFold symbols) ([], -1) (xs >>= getOps)

createMapsFold :: SymbolTable -> Operand -> ([(Operand, Operand)], Int) -> ([(Operand, Operand)], Int)
createMapsFold symbols opr (maps, size) =
  case opr of
    (Pseudo v) -> 
      case lookup opr maps of
        (Just _) -> (maps, size)
        Nothing -> case lookup v symbols of
          -- static var is stored in data section, not stack
          Just (_, StaticAttr init global) -> ((opr, Data v):maps, size)
          _ -> ((opr, Stack size):maps, size - 1)
    _ -> (maps, size)

getOps :: AsmInstr -> [Operand]
getOps x = getDst x ++ getSrcs x

getSrcs :: AsmInstr -> [Operand]
getSrcs x =
  case x of
    AsmMov a b -> [b]
    AsmUnary _ a b -> [b]
    AsmBinary _ a b c -> [b, c]
    AsmCmp a b -> [a, b]
    AsmPush a -> [a]
    _ -> []

getDst :: AsmInstr -> [Operand]
getDst x =
  case x of
    AsmMov a b -> [a]
    AsmUnary _ a b -> [a]
    AsmBinary _ a b c -> [a]
    _ -> []

-- map identifiers used as src/dst to stack locations
replacePseudo :: [(Operand, Operand)] -> AsmInstr -> AsmInstr
replacePseudo maps = mapOps f
  where f op = case lookup op maps of
          (Just newOp) -> newOp
          Nothing -> case op of
            Pseudo s -> error "Compiler Error: Missing map for pseudoregister"
            _ -> op

mapOps :: (Operand -> Operand) -> AsmInstr -> AsmInstr
mapOps f (AsmMov a b) = AsmMov (f a) (f b)
mapOps f (AsmUnary op a b) = AsmUnary op (f a) (f b)
mapOps f (AsmBinary op a b c) = AsmBinary op (f a) (f b) (f c)
mapOps f (AsmCmp a b) = AsmCmp (f a) (f b)
mapOps f (AsmPush a) = AsmPush (f a)
mapOps f x = x

tacValToAsm :: TACVal -> Operand
tacValToAsm (TACLit n) = AsmLit n
tacValToAsm (TACVar s) = Pseudo s