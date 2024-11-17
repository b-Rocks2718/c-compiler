module Main where

import Utils ( Parser(runParser), liftA2, writeResult, showErr, splitOn, Result (Err, Ok), Result(Fail))
import Preprocessor ( preprocess )
import Lexer ( lexProg, showTokens )
import Parser ( parseProg )
import Semantics ( resolve, showSymbols )
import TAC (progToTAC)
import AsmGen ( progToAsm )
import CodeGen ( progToMachine, asmToStr )

import Control.Monad (when)
import System.Environment ( getArgs )

-- run all the compiler stages and write the result

main :: IO Int
main = do
  args <- getArgs
  let path = case args of
        [] -> error "Got no arguments - please pass the file to compile"
        _ -> head args
  let flags = tail args
  content <- readFile path
  let processed = preprocess content
  let tokens = lexProg processed
  when ("-tokens" `elem` flags) $ do
    putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = tokens >>= (runParser parseProg . fst)
  when ("-ast" `elem` flags) $ do
    putStrLn ("\nSyntax tree:\n" ++ show (fst <$> ast))
  let resolved = ast >>= resolve . fst
  let symbols = fst <$> resolved
  when ("-semantics" `elem` flags) $ do
    putStrLn ("\nResolved tree:\n" ++ show (snd <$> resolved))
    putStrLn ("\nSymbol Table:\n" ++ showSymbols symbols)
  let tacRslt = uncurry progToTAC <$> resolved
      tac = snd <$> tacRslt
      tacSymbols = fst <$> tacRslt
  when ("-tac" `elem` flags) $ do
    putStrLn ("\nTAC:\n" ++ show tac)
    putStrLn ("\nSymbol Table:\n" ++ showSymbols tacSymbols)
  let asm = liftA2 progToAsm tac symbols
  when ("-asm" `elem` flags) $ do
    putStrLn ("\nAsmAST:\n" ++ show asm)
  let asm' = progToMachine <$> asm
  let code = unlines . fmap asmToStr <$> asm'
  showErr code
  let fileName = head $ splitOn "." path
  let asmFile = fileName ++ ".s"
  writeResult asmFile code
  case code of
    Ok _ -> return 0
    Err _ -> return 1
    Fail -> return 2