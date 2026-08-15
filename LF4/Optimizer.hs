module Optimizer where

import AbsLF
import Auxiliar
import Control.Monad.State (StateT, get, put, runStateT)
import Data.Generics -- Importar SYB
import Interpreter

-- A travessia genérica substitui optimizeP e optimizeF manuais.
-- 'everywhere' aplica a transformação 'optimizeE' em toda a árvore (bottom-up).
optimizeP :: Program -> Program
optimizeP = everywhere (mkT optimizeE)

-- Função de transformação local.
-- Assume que as sub-expressões (filhos) já estão otimizadas.
optimizeE :: Exp -> Exp
optimizeE exp = case exp of
  EIf (EInt v) expT expE ->
    if v == 0
      then expE
      else expT
  ENot v ->
    if isLiteral v
      then tryEval (ENot v)
      else ENot v
  ECon e1 e2 -> tryFold ECon e1 e2
  EAdd e1 e2 -> tryFold EAdd e1 e2
  ESub e1 e2 -> tryFold ESub e1 e2
  EMul e1 e2 -> tryFold EMul e1 e2
  EDiv e1 e2 -> tryFold EDiv e1 e2
  EOr e1 e2 -> tryFold EOr e1 e2
  EAnd e1 e2 -> tryFold EAnd e1 e2
  -- ECall e outros casos:
  -- Como é bottom-up, os argumentos dentro de ECall já foram otimizados.
  -- Basta retornar a própria expressão.
  _ -> exp

-- Tenta avaliar uma operação binária se ambos os operandos forem literais
-- Como o eval é bottom up, as expressões já foram otimizadas, logo só preciso aplicar o construtor
tryFold :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Exp
tryFold constr e1 e2 =
  let binExp = constr e1 e2
   in if isLiteral e1 && isLiteral e2
        then tryEval binExp -- Faz a avaliação da expressão de literais
        else binExp         -- Não tem como avaliar, só retorno a exp

-- Lógica de avaliação de uma expressão
-- Usado só para avaliação de inteiros e constantes na otimização
tryEval :: Exp -> Exp
tryEval exp =
  let resultR = runStateT (eval exp) ([], []) -- Usa o runState para aplicar o eval no exp
   in case resultR of
        OK (v1, env) -> wrapValueExpression v1 -- Se eval deu certo, transformo o Valor computado em Exp para manter a estrutura do programa
        Erro _ -> exp

-- Funções auxiliares mantidas inalteradas
isLiteral :: Exp -> Bool
isLiteral exp = case exp of
  EStr _ -> True
  ETrue -> True
  EFalse -> True
  EInt _ -> True
  _ -> False

wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse