module Interpreter where

import AbsLF
import AbsLF (Exp (EDiv), Ident)
import AbsLFAux
import Auxiliar
import Control.Monad (Monad (return))
import Control.Monad.Except (MonadError (throwError)) -- Necessário para 'throwError' se for usar R a como base
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans (lift) -- Necessário para usar 'lift' e subir o erro
import Prelude hiding (lookup)

executeP :: Program -> R (Valor, Enviroment)
executeP (Prog fs) = runStateT (eval (expMain fs)) initialEnv
  where
    initialContext = updatecF [] fs
    initialMemory = []
    initialEnv = (initialContext, initialMemory)

    -- Função auxiliar para encontrar a expressão da função 'main'
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

type EnvState a = StateT Enviroment R a -- Monad de estado

type EvalM = EnvState Valor

eval :: Exp -> EnvState Valor
eval x = case x of
  EStr str -> return (ValorStr str)
  ETrue -> return (ValorBool True)
  EFalse -> return (ValorBool False)
  EInt n -> return (ValorInt n)
  EVar id -> do
    (context, mem) <- get
    let v = lookup context id
    return v
  EAdd exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorInt (i v1 + i v2))
  ESub exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorInt (i v1 - i v2))
  EMul exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorInt (i v1 * i v2))
  EDiv exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorInt (i v1 `div` i v2))
  EOr exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorBool (b v1 || b v2))
  EAnd exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorBool (b v1 && b v2))
  ECon exp0 exp -> do
    v1 <- eval exp0
    v2 <- eval exp
    return (ValorStr (s v1 ++ s v2))
  ENot exp -> do
    v1 <- eval exp
    return (ValorBool (not (b v1)))
  EIf exp expT expE -> do
    cond <- eval exp
    case cond of
      ValorInt v ->
        if v /= 0
          then eval expT -- Se verdadeiro (diferente de 0), avalia e retorna expT
          else eval expE -- Se falso (igual a 0), avalia e retorna expE
  ECall id lexp -> do
    -- 1. Antes de tudo faço avaliação dos args
    argValues <- mapM eval lexp -- Usa  o mapM (map monad) para avaliar a lista de argumentos
    (currentCtx, currentMem) <- get -- Salva o contexto e a memórias antes de entrar no escopo da chamada

    -- 2. Verificação do cache
    case lookupECallLog currentMem id argValues of -- Primeiro verifica se a função já foi chamada com os argumentos fornecidos
      Just val -> return val -- Encontrou na memória, só retorno
      Nothing -> do
        -- 3. Construção do contexto para chamada
        let (ValorFun funDef) = lookup currentCtx id
        let parameters = getParams funDef
        let contextFunctions = filter (\(_, v) -> case v of ValorFun _ -> True; _ -> False) currentCtx
        let paramBindings = zip parameters argValues -- Associo cada id a seu valor
        let newContext = paramBindings ++ contextFunctions -- Contexto local para chamada da função

        -- 4. Troca de contexto
        put (newContext, currentMem) -- Troco para o contexto da chamada de função
        resultVal <- eval (getExp funDef) -- Faço eval da função

        -- 5. Atualização da Memória e Restauração do Contexto
        (_, memAfterBody) <- get -- Pego a nova memória
        let finalMem = updateECallMem memAfterBody id argValues resultVal -- Salvo a nova chamada de função feita na memória
        put (currentCtx, finalMem) -- Saio do contexto de chamada de função
        return resultVal -- Retorno o valor da chamada

-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

data Valor
  = ValorInt {i :: Integer}
  | ValorFun {f :: Function}
  | ValorStr {s :: String}
  | ValorBool {b :: Bool}
  deriving (Eq)

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

type RContext = [(Decl, Valor)] -- Contexto de execução: associa Decl (tipo e id) a Valor

type ECallResults = [([Valor], Valor)] -- Lista de parâmetros e o resultado da chamada

type ECallLog = (Ident, ECallResults) -- Id do resultado de cada chamada para um dado args

type ECallMem = [ECallLog]

type Enviroment = (RContext, ECallMem)

lookupECallLog :: ECallMem -> Ident -> [Valor] -> Maybe Valor
lookupECallLog [] _ _ = Nothing
lookupECallLog ((idLog, results) : logs) id args
  | id == idLog = getECallResult results args
  | otherwise = lookupECallLog logs id args

getECallResult :: ECallResults -> [Valor] -> Maybe Valor
getECallResult [] _ = Nothing
getECallResult ((args, result) : logs) args'
  | args == args' = Just result
  | otherwise = getECallResult logs args'

-- Primeiro eu encontro os logs para a função
-- Em seguida eu chamo a função updateResults para atualizar o log
updateECallMem :: ECallMem -> Ident -> [Valor] -> Valor -> ECallMem
updateECallMem [] id args res = [(id, [(args, res)])]
updateECallMem ((idLog, results) : logs) id args res
  | id == idLog = (id, updateResults results args res) : logs
  | otherwise = (idLog, results) : updateECallMem logs id args res

updateResults :: ECallResults -> [Valor] -> Valor -> ECallResults
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
