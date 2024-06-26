
module Assembler where

import Lexer
import AsmGen

import Data.Char
import Data.Binary
import Data.Int
import Data.ByteString (ByteString, pack)
import Control.Applicative

-- assembly can be tokenized with the 'words' function

-- consider changing this function's behavior
posInt :: Parser String Int
posInt = Parser f
  where
    f xs
      | null ns   = Left "empty input"
      | otherwise = Right (read ns, tail xs)
      where (ns, rest) = span isDigit $ head xs

assemble :: String -> ByteString
-- takeWhile removes comments
assemble s = mconcat $ instrToBin <$> (takeWhile (/='#')) <$> lines s

instrToBin :: String -> ByteString
instrToBin = undefined -- something . parseInstr

parseProg :: Parser String [Instruction]
parseProg = many parseInstr

parseInstr :: Parser String Instruction
parseInstr = parseAddi <|> parseAdd

parseAdd :: Parser String Instruction
parseAdd = satisfy (=="add") *> (liftA3 Add parseReg parseReg parseReg)

parseAddi :: Parser String Instruction
parseAddi = satisfy (=="addi") *> (liftA3 Addi parseReg parseReg posInt)

parseReg :: Parser String Reg
parseReg = Parser $ (\ss -> Right (readReg $ head ss, tail ss))

-- use Either for error detection?
readReg :: String -> Reg
readReg "r0" = R0
readReg "r1" = R1
readReg "r2" = R2
readReg "r3" = R3
readReg "r4" = R4
readReg "r5" = R5
readReg "r6" = R6
readReg "r7" = R7
