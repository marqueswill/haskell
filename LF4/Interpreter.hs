module Interpreter where

import AbsLF
import AbsLF (Ident)
import AbsLFAux
import Prelude hiding (lookup)

executeP :: Program -> Valor
executeP (Prog fs) =
  let (v1, env1) = eval initialEnv (expMain fs)
   in v1
  where
    initialContext = updatecF [] fs
    initialMemory = []
    initialEnv = (initialContext, initialMemory)
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

eval :: Enviroment -> Exp -> (Valor, Enviroment)
eval env@(context, mem) x = case x of
  ECon exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorStr (s v1 ++ s v2), env2)
  EAdd exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorInt (i v1 + i v2), env2)
  ESub exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorInt (i v1 - i v2), env2)
  EMul exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorInt (i v1 * i v2), env2)
  EDiv exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorInt (i v1 `div` i v2), env2)
  EOr exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorBool (b v1 || b v2), env2)
  EAnd exp0 exp ->
    let (v1, env1) = eval env exp0
        (v2, env2) = eval env1 exp
     in (ValorBool (b v1 && b v2), env2)
  ENot exp ->
    let (v1, env1) = eval env exp
     in (ValorBool (not (b v1)), env1)
  EStr str -> (ValorStr str, env)
  ETrue -> (ValorBool True, env)
  EFalse -> (ValorBool False, env)
  EInt n -> (ValorInt n, env)
  EVar id -> (lookup context id, env)
  EIf exp expT expE -> case eval env exp of
    (ValorInt v, env1) ->
      if v /= 0
        then eval env1 expT
        else eval env1 expE
  ECall id lexp -> case lookupECallLog mem id lexp of
    Just valor -> (valor, env) -- Se encontrou a chamada na memória, retorno o valor que tava guardado
    Nothing -> eval ecallEnv (getExp funDef) -- Senão, faço a chamada e retorno (v, env)
    where
      (ValorFun funDef) = lookup context id
      parameters = getParams funDef
      -- TODO: fold para propagar mudança no env entre eval dos args
      paramBindings = zip parameters (map (fst . eval env) lexp)
      contextFunctions =
        filter
          ( \(i, v) -> case v of
              ValorFun _ -> True
              _ -> False
          )
          context
      ecallContext = paramBindings ++ contextFunctions
      ecallEnv = (ecallContext, mem)

{- Se eu fosse fazer memorização de resultados de chamada, eu precisaria:
  1) Armazenar os resultados após cada chamada
  2) O que eu preciso armazenar -> id da função, parâmetros informados, resultado da chamada
    -- data Function = Fun Type Ident [Decl] Exp
    -- padrão do context: [Ident, [[Exp], Exp]]
  3) Antes de fazer a chamada, eu primeiro:
    a) verifico se a função já foi chamada antes
    b) comparo as lista de parâmetro com as listas já armazenadas
    c) se as listas forem iguais, eu retorno o resultado que foi guardado
      Caso contrário, eu faço o ecall normalmente e armazeno o novo resultado.
-}

-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

data Valor
  = ValorInt
      { i :: Integer
      }
  | ValorFun
      { f :: Function
      }
  | ValorStr
      { s :: String
      }
  | ValorBool
      { b :: Bool
      }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

type RContext = [(Decl, Valor)] -- Contexto de execução: associa Decl (tipo e id) a Valor

type ECallResults = [([Exp], Valor)] -- Lista de parâmetros e o resultado da chamada

type ECallLog = (Ident, ECallResults) -- Id do resultado de cada chamada para um dado args

type ECallMem = [ECallLog]

type Enviroment = (RContext, ECallMem)

lookupECallLog :: ECallMem -> Ident -> [Exp] -> Maybe Valor
lookupECallLog [] _ _ = Nothing
lookupECallLog ((idLog, results) : logs) id args
  | id == idLog = getECallResult results args
  | otherwise = lookupECallLog logs id args

getECallResult :: ECallResults -> [Exp] -> Maybe Valor
getECallResult [] _ = Nothing
getECallResult ((args, result) : logs) args'
  | args == args' = Just result
  | otherwise = getECallResult logs args'

-- Primeiro eu encontro os logs para a função
-- Em seguida eu chamo a função updateResults para atualizar o log
updateECallMem :: ECallMem -> Ident -> [Exp] -> Valor -> ECallMem
updateECallMem [] id lexp res = [(id, [(lexp, res)])]
updateECallMem ((idLog, results) : logs) id lexp res
  | id == idLog = (id, updateResults results lexp res) : logs
  | otherwise = (idLog, results) : updateECallMem logs id lexp res

updateResults :: ECallResults -> [Exp] -> Valor -> ECallResults
updateResults [] args' res' = [(args', res')]
updateResults ((args, res) : xs) args' res'
  | args == args' = (args, res) : xs
  | otherwise = (args, res) : updateResults xs args' res'

lookup :: RContext -> Ident -> Valor
lookup ((Dec t i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Decl -> Valor -> RContext --
update [] s v = [(s, v)]
update ((d, v) : cs) s nv
  | d == s = (d, nv) : cs -- Se encontrar o decl (tp,id), atualiza o valor
  | otherwise = (d, v) : update cs s nv -- Senão, continua procurando

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\c f -> update c (getDecl f) (ValorFun f)) -- Percorre a lista de funções, atualizando o contexto com cada função e seu valor (ValorFun)
