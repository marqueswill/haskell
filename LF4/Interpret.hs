module Main where

import AbsLF
import ErrM
import Interpreter
import LexLF
import Optimizer
import ParLF
import PrintLF
import Typechecker

main = do
  interact calc
  putStrLn ""

calc soureCode =
  let parserResult = pProgram (myLexer soureCode)
   in case parserResult of
        Ok ast ->
          let typeCheckResult = typeCheckP ast
           in if any isError typeCheckResult
                then show (filter isError typeCheckResult)
                else
                  let optProgram = optimizeP ast
                   in ">>>>>>> Programa original:<<<<<<< \n"
                        ++ printTree ast
                        ++ "\n"
                        ++ ">>>>>>> Programa otimizado:<<<<<<< \n"
                        ++ printTree optProgram
                        ++ "\n"
                        ++ ">>>>>>> Resultado da execucao:<<<<<<< \n"
                        ++ show (executeP optProgram)
        Bad erorMessage -> erorMessage
