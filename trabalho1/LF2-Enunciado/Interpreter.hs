module Interpreter where

import AbsLF
import Prelude hiding (lookup)

{- TODO: Estude a definição do tipo Function no arquivo AbsLF.hs e complete as definicoes 
    de "getParams" e "getExp" abaixo. Note "getName" já é fornecida.         
-}
getType :: Function -> Type
getType (Fun tp name params exp) = tp

getDecl :: Function -> Decl -- Gambiarra pra conseguir colocar a função no novo contexto de execução
getDecl (Fun tp name params exp) =  Dec tp name

getName :: Function -> Ident
getName (Fun tp name params exp) =  name

getParams :: Function -> [Decl]
getParams (Fun tp name params exp) =  params

getExp :: Function -> Exp
getExp (Fun tp name params exp) =  exp


{- : *Não* altere a definição de "executeP" abaixo. 
         *Entenda* a razão da mudança em relação à definição na LI2.
         Garanta que saiba explicar verbalmente isso.
-}

-- Inicializa o contexto de execução com as funções e avalia a expressão do main
executeP :: Program -> Valor
executeP (Prog fs) =  eval (updatecF [] fs) (expMain fs) 
    where expMain (f:xs) -- Procura a função main na lista de funções
              | getName f == Ident "main" =  getExp f
              | otherwise = expMain xs


eval :: RContext -> Exp -> Valor
eval context x = case x of
    ECon exp0 exp  -> ValorStr ( s (eval context exp0) ++  s (eval context exp) )
    EAdd exp0 exp  -> ValorInt ( i (eval context exp0)  +  i (eval context exp))
    ESub exp0 exp  -> ValorInt ( i (eval context exp0)  -  i (eval context exp))
    EMul exp0 exp  -> ValorInt ( i (eval context exp0)  *  i (eval context exp))
    EDiv exp0 exp  -> ValorInt ( i (eval context exp0) `div` i (eval context exp))
    EOr  exp0 exp  -> ValorBool ( b (eval context exp0)  || b (eval context exp))
    EAnd exp0 exp  -> ValorBool ( b (eval context exp0)  && b (eval context exp))
    ENot exp       -> ValorBool ( not (b (eval context exp)))
    EStr str       -> ValorStr str
    ETrue          -> ValorBool True
    EFalse         -> ValorBool False
    EInt n         -> ValorInt n
    EVar id        -> lookup context id -- Vai dar ruim?
{- TODO: remova "undefined" e implemente a avaliação do "EIf" abaixo. A primeira expressao ("exp") é a condição,
   "expT" é a expressão do "then" e "expE" é a expressão do "else". A semântica (comportamento)
   pretendido é o seguinte: compare o valor resultante da avaliação de "exp" com 0.
   se o valor for diferente de 0, retorna-se o resultado da avaliação da expressão expT; 
   caso contrário, retorna-se o resultado da avaliação da expressão expE. 
   @dica: estude a semântica do "SIf" na LI2 e saiba explicar a diferença -}
    EIf exp expT expE -> case eval context exp of 
                          ValorInt  v -> if v /= 0
                                          then eval context expT
                                          else eval context expE
                          -- ValorBool v -> if v
                          --                 then eval context expT
                          --                 else eval context expE

{- TODO: abaixo, troque "undefined" por chamadas das funcoes definidas no inicio do arquivo
    aplicadas ao argumento "funDef"  @dica: não altere o resto, mas saiba explicar o funcionamento -}
    ECall id lexp   -> eval (paramBindings ++ contextFunctions) (getExp funDef)
                          where (ValorFun funDef) = lookup context id
                                parameters =  getParams funDef
                                paramBindings = zip parameters (map (eval context) lexp)
                                contextFunctions = filter (\(i,v) -> case v of
                                                                         ValorFun _ -> True
                                                                         _ -> False
                                                           )
                                                          context



-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

{-
data Valor = ValorInt Integer |
             ValorStr String
i (ValorInt vi) = vi             
s (ValorStr vs) = vs
-}

data Valor = ValorInt {
               i :: Integer
             }
            |
             ValorFun {
               f :: Function
             }
            |
             ValorStr {
               s :: String
             }
            | ValorBool {
               b :: Bool
             }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f
--(\(Ident x) -> x) nf

type RContext = [(Decl,Valor)] -- Contexto de execução: associa Decl (tipo e id) a Valor

lookup :: RContext -> Ident -> Valor
lookup (((Dec t i),v):cs) s
   | i == s = v
   | otherwise = lookup cs s
  
update :: RContext -> Decl -> Valor -> RContext -- 
update [] s v = [(s,v)]
update ((d,v):cs) s nv
  | d == s = (d,nv):cs                 -- Se encontrar o decl (tp,id), atualiza o valor
  | otherwise = (d,v) : update cs s nv -- Senão, continua procurando


updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f:fs) = updatecF (update c (getDecl f) (ValorFun f)) fs -- Percorre a lista de funções, atualizando o contexto com cada função e seu valor (ValorFun)

