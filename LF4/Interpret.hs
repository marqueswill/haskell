module Main where

import AbsLF
import Auxiliar
import Control.Monad (ap) -- Adicionado para caso você queira usar o main com 'interact calc' de forma mais pura
import ErrM
import Interpreter
import LexLF
import Optimizer
import ParLF
import PrintLF
import Typechecker

main :: IO ()
main = do
  interact calc
  putStrLn ""

-- NO Main.hs (dentro da função calc)

calc :: String -> String
calc soureCode =
  let parserResult = pProgram (myLexer soureCode)
   in case parserResult of
        Ok ast ->
          let typeCheckResult = typeCheckP ast
           in if any isError typeCheckResult
                then show (filter isError typeCheckResult)
                else
                  let optProgram = optimizeP ast
                   in case executeP optProgram of
                        -- Corrigido: Usar OK (com 'K' maiúsculo) para a Monad R a
                        OK (val, (_, mem)) ->
                          ">>>>>>> Programa original:<<<<<<< \n"
                            ++ printTree ast
                            ++ "\n"
                            ++ ">>>>>>> Programa otimizado:<<<<<<< \n"
                            ++ printTree optProgram
                            ++ "\n"
                            ++ ">>>>>>> Resultado da execucao:<<<<<<< \n"
                            ++ show val
                            ++ "\n\n"
                            ++ ">>>>>>> Memoria (Cache):<<<<<<< \n"
                            ++ show mem
                        -- Este já estava correto (Erro com 'E' maiúsculo)
                        Erro erorMessage ->
                          ">>>>>>> Erro durante a Execução:<<<<<<< \n"
                            ++ erorMessage
        Bad erorMessage -> erorMessage