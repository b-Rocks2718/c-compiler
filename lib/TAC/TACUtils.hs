module TACUtils where

import AST (Type_(..), Const_ (..))
import TypedAST (SymbolTable, IdentAttrs(..))
import TACAST
import Control.Monad.State

tacDataInit :: SymbolTable -> TACData
tacDataInit symbols = TACData (makeConstant IntType 0) symbols 0

-- create a unique temporary variable name
makeTemp :: String -> Type_ -> TACState Val
makeTemp name type_ = do
  n <- gets getN
  putN (n + 1)
  let varName = name ++ ".tmp." ++ show n
  -- add to symbol table
  symbols <- gets getSymbols
  putSymbols $ (varName, (type_, LocalAttr)) : symbols
  return (Var varName)

putDst :: Val -> TACState ()
putDst dst = do
  TACData _ symbols n <- get
  put (TACData dst symbols n)

putSymbols :: SymbolTable -> TACState ()
putSymbols symbols = do
  TACData dst _ n <- get
  put (TACData dst symbols n)

putN :: Int -> TACState ()
putN n = do
  TACData dst symbols _ <- get
  put (TACData dst symbols n)

makeConstant :: Type_ -> Int -> Val
makeConstant IntType = Constant . ConstInt
makeConstant UIntType = Constant . ConstUInt
makeConstant LongType = Constant . ConstLong
makeConstant ULongType = Constant . ConstULong
makeConstant _ = error "Compiler Error: invalid constant"