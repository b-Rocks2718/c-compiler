
module Main where

import System.Environment
import Data.Maybe
import AParser

data Token = IntLit Int
           | Ident String
           | Void
           | Int_
           | Return
           | OpenP
           | CloseP
           | OpenB
           | CloseB
           | Semi

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  args <- getArgs
  let path = fromMaybe "" $ safeHead args
  content <- readFile path
  return ()
