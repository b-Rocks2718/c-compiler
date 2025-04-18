module ParserUtils where

import Lexer
import AST

isIdent :: Token -> Bool
isIdent (Ident _) = True
isIdent _ = False

isIntLit :: Token -> Bool
isIntLit (IntLit _) = True
isIntLit _ = False

isLongLit :: Token -> Bool
isLongLit (LongLit _) = True
isLongLit _ = False

isUIntLit :: Token -> Bool
isUIntLit (UIntLit _) = True
isUIntLit _ = False

isULongLit :: Token -> Bool
isULongLit (ULongLit _) = True
isULongLit _ = False

isUnaryOp :: Token -> Bool
isUnaryOp op = case op of
  Minus -> True
  Tilde -> True
  Exclamation -> True
  _ -> False

isBinOp :: Token -> Bool
isBinOp op =
  case op of
    Minus -> True
    Plus -> True
    Asterisk -> True
    Slash -> True
    Percent -> True
    Ampersand -> True
    Pipe -> True
    Carat -> True
    ShiftLTok -> True
    ShiftRTok -> True
    DoubleAmpersand -> True
    DoublePipe -> True
    DoubleEquals -> True
    NotEqual -> True
    GreaterThan -> True
    LessThan -> True
    GreaterThanEq -> True
    LessThanEq -> True
    Equals -> True
    PlusEq -> True
    MinusEq -> True
    TimesEq -> True
    DivEq -> True
    ModEq -> True
    AndEq -> True
    OrEq -> True
    XorEq -> True
    ShlEq -> True
    ShrEq -> True
    Question -> True -- ternary operator can be parsed like a binary operator
    _ -> False

getUnaryOp :: Token -> UnaryOp
getUnaryOp op = case op of
  Tilde -> Complement
  Minus -> Negate
  Exclamation -> BoolNot
  _ -> error $ show op ++ " is not an unary operator"

tokenToBinOp :: Token -> BinOp
tokenToBinOp op = case op of
  Plus -> AddOp
  Minus -> SubOp
  Asterisk -> MulOp
  Slash -> DivOp
  Percent -> ModOp
  Ampersand -> BitAnd
  Pipe -> BitOr
  Carat -> BitXor
  ShiftLTok -> BitShl
  ShiftRTok -> BitShr
  DoubleAmpersand -> BoolAnd
  DoublePipe -> BoolOr
  DoubleEquals -> BoolEq
  NotEqual -> BoolNeq
  GreaterThan -> BoolGe
  LessThan -> BoolLe
  GreaterThanEq -> BoolGeq
  LessThanEq -> BoolLeq
  Equals -> AssignOp
  PlusEq -> PlusEqOp
  MinusEq -> MinusEqOp
  TimesEq -> TimesEqOp
  DivEq -> DivEqOp
  ModEq -> ModEqOp
  AndEq -> AndEqOp
  OrEq -> OrEqOp
  XorEq -> XorEqOp
  ShlEq -> ShlEqOp
  ShrEq -> ShrEqOp
  Question -> TernaryOp
  _ -> error $ show op ++ " is not a binary operator"

getCompoundOp :: BinOp -> BinOp
getCompoundOp op = case op of
  PlusEqOp -> AddOp
  MinusEqOp -> SubOp
  TimesEqOp -> MulOp
  DivEqOp -> DivOp
  ModEqOp -> ModOp
  AndEqOp -> BitAnd
  OrEqOp -> BitOr
  XorEqOp -> BitXor
  ShlEqOp -> BitShl
  ShrEqOp -> BitShr
  _ -> error $ show op ++ " is not a compound binary operator"

getPrec :: BinOp -> Int
getPrec op = case op of
  MulOp -> 50
  DivOp -> 50
  ModOp -> 50
  AddOp -> 45
  SubOp -> 45
  BitShl -> 40
  BitShr -> 40
  BoolLe -> 35
  BoolGe -> 35
  BoolLeq -> 35
  BoolGeq -> 35
  BoolEq -> 30
  BoolNeq -> 30
  BitAnd -> 25
  BitXor -> 20
  BitOr -> 15
  BoolAnd -> 10
  BoolOr -> 5
  TernaryOp -> 3
  AssignOp -> 1
  PlusEqOp -> 1
  MinusEqOp -> 1
  TimesEqOp -> 1
  DivEqOp -> 1
  ModEqOp -> 1
  AndEqOp -> 1
  OrEqOp -> 1
  XorEqOp -> 1
  ShlEqOp -> 1
  ShrEqOp -> 1

isFunc :: Type_ -> Bool
isFunc (FunType _ _) = True
isFunc _ = False

-- number of 16 bit words needed to store each type
typeSize :: Type_ -> Int
typeSize IntType = 1
typeSize UIntType = 1
typeSize LongType = 2
typeSize ULongType = 2
typeSize (PointerType _) = 1
typeSize (FunType _ _) = 
  error "Compiler Error: function type does not have size"
typeSize (ArrayType inner size) = size * typeSize inner
  
isSigned :: Type_ -> Bool
isSigned IntType = True
isSigned UIntType = False
isSigned LongType = True
isSigned ULongType = False
isSigned (PointerType _) = False
isSigned (FunType _ _) = 
  error "Compiler Error: function type is not signed/unsigned"
isSigned (ArrayType _ _) = 
  error "Compiler Error: array type is not signed/unsigned"