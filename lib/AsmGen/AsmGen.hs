module AsmGen where

import TypedAST(SymbolTable, IdentAttrs(..), Const_ (getConstInt))
import qualified TACAST
import AsmAST
import Utils

progToAsm :: TACAST.Prog -> SymbolTable -> Prog
progToAsm (TACAST.Prog p) symbols = Prog (topLevelToAsm symbols <$> p)

topLevelToAsm :: SymbolTable -> TACAST.TopLevel -> TopLevel
topLevelToAsm symbols (TACAST.Func name global params body) =
  Func name global $ replacePseudo maps <$> instrs
  where (maps, size) = createMaps instrs symbols
        instrs = AllocateStack (size + 1)  : paramsToAsm params ++ (body >>= exprToAsm)
topLevelToAsm _ (TACAST.StaticVar name global _ init_) = StaticVar name global init_
topLevelToAsm _ (TACAST.Comment s) = Comment s

paramsToAsm :: [String] -> [Instr]
paramsToAsm ps = getZipList (ZipList copyList <*> ZipList ps)
  where copyList =
          [\s -> Mov (Pseudo s) (Reg R3),
           \s -> Mov (Pseudo s) (Reg R4),
           \s -> Mov (Pseudo s) (Reg R5),
           \s -> Mov (Pseudo s) (Reg R6)] ++
           ((\n s -> Mov (Pseudo s) (Memory bp n)) <$> [2..])

exprToAsm :: TACAST.Instr -> [Instr]
exprToAsm instr =
  case instr of
    (TACAST.Return val) -> [Mov (Reg R3) (tacValToAsm val), Ret]
    (TACAST.Copy dst src) -> [Mov (tacValToAsm dst) (tacValToAsm src)]
    (TACAST.Unary op dst src) ->
      [Unary op (tacValToAsm dst) (tacValToAsm src)]
    (TACAST.Binary op dst src1 src2 type_) ->
      [Binary op (tacValToAsm dst) (tacValToAsm src1) (tacValToAsm src2) type_]
    (TACAST.CondJump cond label) -> [CondJump cond label]
    (TACAST.Cmp val1 val2) -> [Cmp (tacValToAsm val1) (tacValToAsm val2)]
    (TACAST.Jump label) -> [Jump label]
    (TACAST.Label s) -> [Label s]
    (TACAST.Call name dst srcs) ->
      -- push in reverse order
      getZipList (ZipList stackList <*> ZipList (reverse stackArgs)) ++
      getZipList (ZipList regList <*> ZipList regArgs) ++
      [Call name,
      AllocateStack (length stackArgs), -- deallocate stack
      Mov (tacValToAsm dst) (Reg R3)]
      where regList =
              [Mov (Reg R3) . tacValToAsm,
               Mov (Reg R4) . tacValToAsm,
               Mov (Reg R5) . tacValToAsm,
               Mov (Reg R6) . tacValToAsm]
            regArgs = take 4 srcs
            stackArgs = drop 4 srcs
            stackList = repeat (Push . tacValToAsm)
    (TACAST.GetAddress dst src) -> [GetAddress (tacValToAsm dst) (tacValToAsm src)]
    (TACAST.Load dst ptr) -> [Mov (Reg R3) (tacValToAsm ptr),
                              Mov (Reg R4) (Memory R3 0),
                              Mov (tacValToAsm dst) (Reg R4)]
    (TACAST.Store ptr src) -> [Mov (Reg R3) (tacValToAsm ptr),
                               Mov (Reg R4) (tacValToAsm src),
                              Mov (Memory R3 0) (Reg R4)]
    (TACAST.CopyToOffset dst src offset) -> [Mov (makePseudoMem dst offset) (tacValToAsm src)]

createMaps :: [Instr] -> SymbolTable -> ([(Operand, Operand)], Int)
createMaps xs symbols = foldr (createMapsFold symbols) ([], -1) (xs >>= getOps)

createMapsFold :: SymbolTable -> Operand -> ([(Operand, Operand)], Int) -> ([(Operand, Operand)], Int)
createMapsFold symbols opr (maps, size) =
  case opr of
    (Pseudo v) ->
      case lookup opr maps of
        (Just _) -> (maps, size)
        Nothing -> case lookup v symbols of
          -- static var is stored in data section, not stack
          Just (_, StaticAttr _ _) -> ((opr, Data v):maps, size)
          _ -> ((opr, Memory bp size):maps, size - 1)
    (PseudoMem v _) ->
      case lookup opr maps of
        (Just _) -> (maps, size)
        Nothing -> case lookup v symbols of
          -- static var is stored in data section, not stack
          Just (_, StaticAttr _ _) -> ((opr, Data v):maps, size)
          _ -> ((opr, Memory bp size):maps, size - 1)
    _ -> (maps, size)

getOps :: Instr -> [Operand]
getOps x = getDst x ++ getSrcs x

getSrcs :: Instr -> [Operand]
getSrcs x =
  case x of
    Mov _ b -> [b]
    Unary _ _ b -> [b]
    Binary _ _ b c _ -> [b, c]
    Cmp a b -> [a, b]
    Push a -> [a]
    GetAddress _ b -> [b]
    _ -> []

getDst :: Instr -> [Operand]
getDst x =
  case x of
    Mov a _ -> [a]
    Unary _ a _ -> [a]
    Binary _ a _ _ _ -> [a]
    GetAddress a _ -> [a]
    _ -> []

-- map identifiers used as src/dst to stack locations
replacePseudo :: [(Operand, Operand)] -> Instr -> Instr
replacePseudo maps = mapOps f
  where f op = case lookup op maps of
          (Just newOp) -> newOp
          Nothing -> case op of
            Pseudo _ -> error "Compiler Error: Missing map for pseudoregister"
            _ -> op

mapOps :: (Operand -> Operand) -> Instr -> Instr
mapOps f (Mov a b) = Mov (f a) (f b)
mapOps f (Unary op a b) = Unary op (f a) (f b)
mapOps f (Binary op a b c type_) = Binary op (f a) (f b) (f c) type_
mapOps f (Cmp a b) = Cmp (f a) (f b)
mapOps f (Push a) = Push (f a)
mapOps f (GetAddress a b) = GetAddress (f a) (f b)
mapOps _ x = x

tacValToAsm :: TACAST.Val -> Operand
tacValToAsm (TACAST.Constant n) = Lit $ getConstInt n
tacValToAsm (TACAST.Var s) = Pseudo s

makePseudoMem :: TACAST.Val -> Int -> Operand
makePseudoMem (TACAST.Var s) = PseudoMem s
makePseudoMem _ = error "Compiler Error: attempted to make PseudoMem for non-var"