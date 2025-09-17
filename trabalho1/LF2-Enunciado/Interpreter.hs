module Interpreter where

import AbsLF
import Prelude hiding (lookup)

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
    EVar id        -> lookup context id
    EIf exp expT expE -> case eval context exp of 
                          ValorInt  v -> if v /= 0
                                          then eval context expT
                                          else eval context expE
    ECall id lexp   -> eval (paramBindings ++ contextFunctions) (getExp funDef)
                          where (ValorFun funDef) = lookup context id
                                parameters =  getParams funDef
                                paramBindings = zip parameters (map (eval context) lexp)
                                contextFunctions = filter onlyFun context
                                onlyFun = (\(i,v) -> case v of
                                                          ValorFun _ -> True
                                                          _ -> False
                                            )



-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

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

