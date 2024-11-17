
module Semantics (resolve, showSymbols) where

import Utils

import qualified AST
import qualified TypedAST
import TypedAST (SymbolTable)
import Typechecking (typecheck)
import IdentifierResolution (resolveFileScopeDclr)
import LoopResolution (labelLoops)
import SwitchResolution (resolveSwitches)
import GotoResolution (resolveGotos)

-- run all the passes in the semantic analysis stage
resolve :: AST.Prog -> Result (SymbolTable, TypedAST.Prog)
resolve (AST.Prog p) = do
  rslt <- AST.Prog <$> resolveProgFold p
  evalStateT (typecheck rslt) []

-- resolve all statements, while tracking the state of the variable map
resolveProgFold :: [AST.Declaration] -> Result [AST.Declaration]
resolveProgFold fs = evalStateT (foldr f (pure []) fs) ([], 0)
  where f x xs = do
          resolvedIdentifiers <- resolveFileScopeDclr x
          let resolvedLoops = labelLoops resolvedIdentifiers
              resolvedSwitches = resolvedLoops >>= resolveSwitches
              resolvedGotos = resolvedSwitches >>= resolveGotos
          dclrs <- xs
          case resolvedGotos of
            Ok dclr -> return (dclr : dclrs)
            Err msg -> lift (Err msg)
            Fail -> lift Fail

showSymbols :: Result SymbolTable -> String
showSymbols (Ok symbols) =
  symbols >>= (\(ident, attrs) -> show ident ++ ": " ++ show attrs ++ "\n")
showSymbols (Err msg) = msg
showSymbols Fail = "Fail"