module Main where

import LexLI
import ParLI
import AbsLI
import Interpreter

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok p = pProgram  (myLexer s) 
  in show (executeP p)
