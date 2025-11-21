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
                      -- Captura o resultado (val) e o ambiente ((_, mem))
                      (val, (_, mem)) = executeP optProgram
                   in ">>>>>>> Programa original:<<<<<<< \n"
                        ++ printTree ast
                        ++ "\n"
                        ++ ">>>>>>> Programa otimizado:<<<<<<< \n"
                        ++ printTree optProgram
                        ++ "\n"
                        ++ ">>>>>>> Resultado da execucao:<<<<<<< \n"
                        ++ show val
                        ++ "\n\n"
                        ++ ">>>>>>> Memoria (Cache):<<<<<<< \n"
                        ++ show mem -- Exibe a lista de chamadas memorizadas
        Bad erorMessage -> erorMessage