
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

showEither :: Either String String -> String
showEither (Left s) = s
showEither (Right s) = s

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
  putStrLn ("\nAsm tree:\n" ++ show asmAst)
  let code = progToCode <$> asmAst
  putStrLn ("\nAsm code:\n" ++ showEither code)
  let outPath = (head $ splitOn "." path) ++ ".s"
  writeFile outPath (showEither code) -- change this later
  -- not a good idea to write a file if there's an error
