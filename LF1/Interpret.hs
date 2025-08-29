module Main where

{-
import LexLF
import ParLF
import AbsLF
-}
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok p = pProgram  (myLexer s) 
  in show (executeP p)
