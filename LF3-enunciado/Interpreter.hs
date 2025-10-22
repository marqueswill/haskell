module Interpreter where

import AbsLF
import AbsLFAux  -- TODO: leia agora o conteudo desse arquivo (AbsLFAux.hs) e explique por que refatoramos assim 
import Prelude hiding (lookup)


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
    lambda@(ELambda params exp) -> undefined
    -- TODO: em EComp abaixo, troque undefined (2 ocorrencias) pela construcao apropriada                           
    EComp exp1 exp2 ->  let (ValorFun exp1') = eval context exp1
                            (ValorFun exp2') = undefined in 
                          ValorFun(ELambda (getParamsTypesL exp2') 
                                           (ECall undefined [ECall exp2' (getParamsExpL exp2')]))             
    {- TODO: em ECall abaixo, troque undefined (3 ocorrencias) pela construcao apropriada.                           
       Dica: estude o codigo, buscando entender tambem as definicoes locais -}
    ECall exp lexp ->  if (length lexp < length parameters) 
                         then ValorFun (ELambda undefined undefined) -- TODO: que caso eh esse ?
                         else eval (paramBindings ++ contextFunctions) exp' -- TODO: que caso eh esse ? 
                        where (ValorFun lambda) = eval context undefined
                              parameters = getParamsL lambda
                              paramBindings = zip parameters (map (eval context) lexp)
                              params' = drop (length lexp) (getParamsTypesL lambda)
                              exp' = subst paramBindings (getExpL lambda) 
                              contextFunctions = filter (\(i,v) -> case v of 
                                                                         ValorFun _ -> True 
                                                                         _ -> False
                                                           ) 
                                                          context


-- a função "subst" gera uma nova expressao a partir dos bindings em RContext
subst :: RContext -> Exp -> Exp 
subst rc exp  = case exp of  
    EVar id        -> bind id rc -- TODO: por que eh implementado assim ?
    -- TODO: explique a implementacao da linha abaixo
    lambda@(ELambda paramsTypes exp) -> ELambda paramsTypes (subst (rc `diff` (getParamsL lambda)) exp)
    ECall exp lexp -> ECall (subst rc exp ) (map (subst rc) lexp)
    EAdd exp0 exp  -> EAdd (subst rc exp0 ) (subst rc exp )
    -- TODO: nos casos abaixo, troque cada undefined pela construcao apropriada
    EComp exp1 exp2 -> undefined
    EIf expC expT expE -> undefined
    ECon exp0 exp  -> undefined
    ESub exp0 exp  -> undefined
    EMul exp0 exp  -> undefined
    EDiv exp0 exp  -> undefined
    EOr  exp0 exp  -> undefined
    EAnd exp0 exp  -> undefined
    ENot exp       -> undefined
    _ -> exp   -- TODO: quais sao esses casos e por que sao implementados assim ?                        

{- TODO: 
  sobre a implementacao finalizada de subst:
  1) qual eh o caso base?
  2) como descrever o numero de casos recursivos? depende (in)diretamente de algo?
  3) qual a finalidade dos casos recursivos?
  4) por que a linha 64 eh diferente dos outros casos recursivos?  
  5) numa especificacao textual intuitiva e concisa (semelhante ao comentario na linha 59),
     qual a linha mais importante entre 62-77 ?
  6) Ha semelhanca de implementacao em relacao ao Optimizer.hs? Qual(is)?    
-}

-- a função "diff" faz a diferença, tirando de RContext os mapeamentos envolvendo [Ident].
diff :: RContext -> [Ident] -> RContext
rc `diff` [] = rc
[] `diff` _ = [] 
((k,v):kvs) `diff` (id:ids) 
    | k == id =  kvs `diff` ids
    | otherwise = (k,v) : ( kvs `diff` (id:ids))

-- a função bind retorna uma expressao contendo o valor do id no RContext, ou o proprio id. 
-- TODO: por que nao usamos o lookup no lugar de bind ?
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
            | 
             ValorFun {
               f :: Exp   --f :: Function  **NOVO TODO: Por que mudou ?
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
  show (ValorFun f) = show f  -- TODO: por que essa linha funciona ?
  

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
-- updatecF c (f:fs) = updatecF (update c (getName f) (ValorFun f)) fs
