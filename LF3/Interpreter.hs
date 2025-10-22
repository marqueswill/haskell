{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use foldl" #-}
module Interpreter where

import AbsLF
import AbsLFAux  -- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim 
import Prelude hiding (lookup)                                {- Essa refatoração foi feita em um arquivo auxilar por que o arquivo da sintaxe abastrata é gerado pelo bnfc 
                                                                -> Abstração dos getters
                                                                -> Modularidade do código
                                                                -> Organização e legibilidade
                                                                -> Reuso de funções em diversas partes (menos código duplicado)
                                                              -}
executeP :: Program -> Valor
executeP (Prog fs) =  eval (updatecF [] fs) (expMain fs)
    where expMain (f:xs) 
              | (getName f == (Ident "main")) =  getExp f
              | otherwise = expMain xs                                            

type RContext = [(Ident,Valor)]
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
    EVar id        -> lookup context  id
    EIf exp expT expE -> if ( i (eval context exp) /= 0) 
                          then eval context expT
                          else eval context expE

    -- TODO: na linha abaixo, retorne um ValorFun contendo o lambda e saiba explicar a razao 
    -- O eval retorna uma função lamda como valor para que ela possa ser aplicada posteriormente
    -- Por exemplo, em ECall quando ocorre uma avaliação parcial, retornamos o valor de uma função lambda
    lambda@(ELambda params exp) -> ValorFun lambda

    -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada
    -- Considerando que exp1 e exp2 são as expressões que abstraem duas funções 'f' e 'g' quaisquer                           
    EComp exp1 exp2 ->  let (ValorFun exp1') = eval context exp1 -- Avalia a expressão que define f
                            (ValorFun exp2') = eval context exp2 -- Avalia a expressão que define g
                            ecallCompResult = ECall exp1' [ECall exp2' (getParamsExpL exp2')] -- "f(g(x))" ==> Faz a chamada da função 'f' usando o resultado da função 'g' como argumento
                        in 
                          ValorFun (ELambda (getParamsTypesL exp2') ecallCompResult) {- Retorna uma função como valor a qual é a composição <f . g>
                                                                                          Essa função tem os parametros de g, e tem como corpo a expressão resultante da aplicação de f no resultado de g -}

    {- TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.                           
       Dica: estude o codigo, buscando entender tambem as definicoes locais -}
    ECall exp lexp -> if (length lexp < length parameters) then  -- Se tenho menos argumentos que parâmetros 
                        -- TODO: que caso eh esse? ==> APLICAÇÃO PARCIAL: retorno uma função lambda
                        ValorFun (ELambda params' exp')                             
                      else 
                        -- TODO: que caso eh esse? ==> APLICAÇÃO TOTAL: retorno o resultado da expressão
                        eval (paramBindings ++ contextFunctions) exp'                
                      
                      where (ValorFun lambda) = eval context exp
                            parameters = getParamsL lambda                           -- Parâmetros da função lambda
                            paramBindings = zip parameters (map (eval context) lexp) -- Associo os parâmtros da função aos argumentos passados
                            contextFunctions = filter (\(i,v) -> case v of           -- Contexto onde está as definições das funções
                                                                        ValorFun _ -> True 
                                                                        _ -> False
                                                          ) 
                                                        context
                            -- Usados na aplicação parcial
                            params' = drop (length lexp) (getParamsTypesL lambda)    -- Removo os parâmetros cujos valores foram fornecidos
                            exp' = subst paramBindings (getExpL lambda)              -- Substituo na expressão os argumentos que foram fornecidos


-- a função "subst" gera uma nova expressao a partir dos bindings em RContext
subst :: RContext -> Exp -> Exp 
subst rc exp  = case exp of  
    -- TODO: Por que eh implementado assim? ==> Porque quando encontramos uma variável, precisamos substituí-la pelo valor associado no contexto de substituição (RContext). 
    EVar id        -> bind id rc 

    {- TODO: Explique a implementacao da linha abaixo ==> 
        Na substituição de valores na expressão lambda, ignoramos os parâmetros da lambda 
        Ex: lambda int x -> x + 2 (basicamente ignoramos o x antes de '->', pq ele é só um parâmetro) -}
    lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` (getParamsL lambda)) exp)
    ECall exp lexp -> ECall (subst rc exp) (map (subst rc) lexp)
    EAdd exp0 exp -> EAdd (subst rc exp0) (subst rc exp)

    -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada
    EComp exp1 exp2 -> EComp (subst rc exp1 ) (subst rc exp2)
    EIf expC expT expE -> EIf (subst rc expC) (subst rc expT) (subst rc expE)
    ECon exp0 exp -> ECon (subst rc exp0) (subst rc exp)
    ESub exp0 exp -> ESub (subst rc exp0) (subst rc exp)
    EMul exp0 exp -> EMul (subst rc exp0) (subst rc exp)
    EDiv exp0 exp -> EDiv (subst rc exp0) (subst rc exp)
    EOr  exp0 exp -> EOr  (subst rc exp0) (subst rc exp)
    EAnd exp0 exp -> EAnd (subst rc exp0) (subst rc exp)
    ENot exp -> ENot (subst rc exp)
    _ -> exp   {- TODO: quais sao esses casos e por que sao implementados assim ? ==> Casos base: EInt, EStr, ETrue, EFalse. 
                                                                                      São implementados assim pois são literais, isto é: não há variáveis para substituir nessas expressões.-}                    

{- TODO: 
  Sobre a implementacao finalizada de subst:
  1) Qual eh o caso base? 
    O caso base são as expressões literais (EInt, EStr, ETrue, EFalse) 
    onde não há variáveis para substituir.

  2) Como descrever o número de casos recursivos? Depende (in)diretamente de algo?
    Depende diretamente da estrutura da expressão. Cada construtor de expressão que contém sub-expressões (como EAdd, EIf, ECall, etc.) 
    requer um caso recursivo para processar essas sub-expressões.

  3) Qual a finalidade dos casos recursivos?
    Processar e substituir variáveis em todas as sub-expressões de uma expressão composta.

  4) Por que a linha 82 eh diferente dos outros casos recursivos?  
    Porque em uma expressão lambda, os parâmetros da função são variáveis ligadas que não devem ser substituídas. 
    Portanto, ao processar o corpo da lambda, removemos essas variáveis do contexto de substituição usando a função diff.

  5) Numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 76),
  qual a linha mais importante entre 80-84?
    Entre o Evar, o Ecall e o Elambda, a linha mais importante é a do EVar porque é o caso base da substituição, 
    onde a variável é efetivamente substituída pelo valor associado no contexto.
    "No caso EVar, substitui-se a variável pelo seu valor associado no contexto de substituição (RContext)."

  6) Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?    
    Ambos utilizam recursão para processar a estrutura da expressão.
    Ambos possuem casos base para expressões literais.
-}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = [] 
((k,v):kvs) `diff` (id:ids) 
    | k == id =  kvs `diff` ids
    | otherwise = (k,v) : ( kvs `diff` (id:ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id. 
{- TODO: por que nao usamos o lookup no lugar de bind? ==> Porque o lookup retorna apenas o valor associado ao id fornecido, 
                                                           enquanto o bind retorna a expressão com o valor associado ao id empacotado, 
                                                           ou o próprio id se não estiver no contexto passado. -}
bind :: Ident -> RContext -> Exp
bind id [] = EVar id  -- retorna o proprio id se ele nao esta ligado em RContext
bind id ((k,v):kvs)
    | k == id = wrapValueExpression v 
    | otherwise = bind id kvs 

-- "wrapValueExpression" empacota um valor em uma expressao 
wrapValueExpression :: Valor -> Exp 
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse
wrapValueExpression (ValorFun exp) = exp 


data Valor = ValorInt {
               i :: Integer         
             }
           | ValorFun {
               f :: Exp   --f :: Function  **NOVO TODO: Por que mudou ? Porque funções agora são valores além de serem expressões
            }   
           | ValorStr {
               s :: String
            } 
           | ValorBool {
               b :: Bool
            }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f  -- TODO: por que essa linha funciona? ==> Porque funções são valores e expressões, é o tipo Exp possui herança de Show
  

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv


-- NOVO: TODO: explique a mudanca em updatecF
updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f:fs) = updatecF (update c (getName f)    
                                       (ValorFun (ELambda (getParams f) (getExp f)))) 
                              fs
