
module CodeGen where

import System.Environment
import Data.List.Split

import Lexer
import Parser
import AsmGen

progToCode :: AsmProg -> String
progToCode (AsmProg f) = funcToCode f

funcToCode :: AsmFunc -> String
funcToCode (AsmFunc name body) =
  name ++ ":\n" ++ (body >>= show)

showEither :: (Show a, Show b) => Either a b -> String
showEither (Left s) = show s
showEither (Right s) = show s

showEitherStr :: Either String String -> String
showEitherStr (Left s) = s
showEitherStr (Right s) = s

writeEither :: String -> Either a String -> IO ()
writeEither path (Right s) = writeFile path s
writeEither _ (Left _) = return ()

main :: IO ()
main = do
  args <- getArgs
  let path = head args
  content <- readFile path
  putStrLn ("Input program:\n" ++ content)
  let processed = preprocess content
  putStrLn ("Preprocessed code:\n" ++ processed)
  let tokens = lexerEval processed
  putStrLn ("\nTokens:\n" ++ showTokens tokens)
  let ast = (fst <$> tokens) >>= (runParser parseProgram)
  putStrLn ("\nSyntax tree:\n" ++ showAST ast)
  let asmAst = progToAsm <$> fst <$> ast
  putStrLn ("\nAsm tree:\n" ++ showEither asmAst)
  let code = progToCode <$> asmAst
  putStrLn ("\nAsm code:\n" ++ showEitherStr code)
  let fileName = (head $ splitOn "." path)
  let asmFile = fileName ++ ".s"
  writeEither asmFile code
  -- let binFile = fileName ++ ".bin"
  -- let binCode = asmToBin  <$> code
  -- writeEither binFile binCode
