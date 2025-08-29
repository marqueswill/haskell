module Interpreter where

import AbsLF
import Prelude hiding (lookup)

executeP :: Program -> Valor
executeP (Prog fs) =  eval (updatecF [] fs) (expMain fs)
    where expMain ((Fun (Ident "main") decls exp):xs) = exp
          expMain ( _ :xs) = expMain xs
  

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
    -- Pattern match booleano
    EIf exp expT expE -> case eval context exp of 
                          ValorInt  v -> if v /= 0
                                          then eval context expT
                                          else eval context expE
                          -- ValorBool v -> if v
                          --                 then eval context expT
                          --                 else eval context expE

    ECall id lexp   -> eval (paramBindings++contextFunctions) exp 
                          where ValorFun (Fun _ decls exp) = lookup context id
                                paramBindings = zip decls (map (eval context) lexp)
                                contextFunctions = filter isFunct context
                                isFunct (_,v) = case v of
                                                  ValorFun _ -> True
                                                  _ -> False


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
updatecF c (f@(Fun id params exp):fs) = updatecF (update c id (ValorFun f)) fs