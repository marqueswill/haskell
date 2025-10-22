module Main where

import LexLI
import ParLI
import AbsLI
import Typechecer

import ErrM

main = do
  interact calc
  putStrLn ""

calc s = 
  let Ok p = pProgram  (myLexer s) 
  in show (typeCheckP p)
