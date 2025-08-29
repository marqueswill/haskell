module Interpreter where

import AbsLI
import Prelude hiding (lookup)

executeP :: Program -> RContext

executeP (Prog fs) =  execute (updatecF [] fs) ( SBlock (stmMain fs))
    where stmMain ((Fun (Ident "main") decls stms):xs) = stms
          stmMain ( _ :xs) = stmMain xs                                            
   
execute :: RContext -> Stm -> RContext
execute context x = case x of
   SAss id exp -> update context id (eval context exp)
   SBlock [] -> context
   SBlock (s:stms) -> execute (execute context s) (SBlock stms) 
   SWhile exp stm -> if ( i (eval context exp) /= 0) 
                      then execute (execute context stm) (SWhile exp stm)
                      else context
   SReturn exp ->  update context  (Ident "return")  (eval context exp)
   SIf exp stmT stmE -> if ( i (eval context exp) /= 0) 
                          then execute context stmT
                          else execute context stmE


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
    Call id lexp   -> lookup (execute (paramBindings++contextFunctions) (SBlock stms)) 
                             (Ident "return")
                          where ValorFun (Fun _ decls stms) = lookup context id
                                paramBindings = zip decls (map (eval context) lexp)
                                contextFunctions = filter (\(i,v) -> case v of 
                                                                         ValorFun _ -> True 
                                                                         _ -> False
                                                           ) 
                                                          context
{- ATENCAO: O CÓDIGO ABAIXO ESTÁ COMENTADO PORQUE O CONTEXTO PASSADO NA CHAMADA DE "execute"
   TEM VARIAVEIS LOCAIS DA FUNCAO QUE ESTA FAZENDO A CHAMADA, O QUE GERA ACOPLAMENTO INDESEJADO ENTRE FUNCOES
    Call id lexp   -> lookup (execute (paramBindings++context) (SBlock stms)) 
                             (Ident "return")
                          where ValorFun (Fun _ decls stms) = lookup context id
                                paramBindings = zip decls (map (eval context) lexp)
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
  show (ValorFun (Fun nf decls _)) = show (nf) ++ "[" ++ show (decls) ++ "]" 
--(\(Ident x) -> x) nf

type RContext = [(Ident,Valor)]

lookup :: RContext -> Ident -> Valor
lookup ((i,v):cs) s
   | i == s = v
   | otherwise = lookup cs s

update :: RContext -> Ident -> Valor -> RContext
update [] s v = [(s,v)]
update ((i,v):cs) s nv
  | i == s = (i,nv):cs
  | otherwise = (i,v) : update cs s nv


updatecF :: RContext -> [Function] -> RContext
updatecF c [] = c
updatecF c (f@(Fun id params stms):fs) = updatecF (update c id (ValorFun f)) fs