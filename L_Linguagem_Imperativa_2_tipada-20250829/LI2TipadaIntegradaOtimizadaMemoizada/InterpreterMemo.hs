module InterpreterMemo where

import AbsLI
import Prelude hiding (lookup)
import PrintLI

executeP :: Program -> Environment

executeP (Prog fs) =  execute (updatecF ([],[],[]) fs) (SBlock (stmMain fs) )
    where stmMain ((Fun _ (Ident "main") decls stms):xs) = stms
          stmMain ( _ :xs) = stmMain xs                                            

                  
execute :: Environment -> Stm -> Environment
execute environment x = case x of
   SDec (Dec tp id) -> updateShallowValue environment id (initVal tp)
   SAss id exp -> let (nvalue, nenv) = eval environment exp in  
                           updateDeepValue nenv id nvalue
-- SDecls tp id ids -> 
-- SInit tp id exp  ->  
-- SDWhile stm  exp -> 
   SBlock [] -> environment
   SBlock (sb@(SBlock bls):stms) -> execute (pop (execute (push environment) sb)) (SBlock stms)
   SBlock (s:stms) -> execute (execute environment s) (SBlock stms) 
   SWhile exp stm -> if ( i (fst(eval environment exp)) /= 0) 
                      then execute (execute environment stm) (SWhile exp stm)
                      else environment
   SReturn exp -> let (nvalue, nenv) = eval environment exp in 
                      updateShallowValue nenv (Ident "return") nvalue
   SIf exp stmT stmE -> if ( i (fst(eval environment exp)) /= 0) 
                          then execute environment stmT
                          else execute environment stmE


eval :: Environment -> Exp -> (Valor,Environment)
eval environment@(localEnv,globalEnv,fcEnv) x = case x of
    ECon exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorStr ((s v1) ++ (s v2)), env2)
    EAdd exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorInt ((i v1) + (i v2)), env2)
    ESub exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorInt ((i v1) - (i v2)), env2)
    EMul exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorInt ((i v1) * (i v2)), env2)
    EDiv exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorInt ((i v1) `div` (i v2)), env2)
    EOr  exp0 exp  -> let (v1,env1) = eval environment exp0
                          (v2,env2) = eval env1 exp in 
                               (ValorBool ((b v1) || (b v2)), env2)
    EAnd exp0 exp  ->  let (v1,env1) = eval environment exp0
                           (v2,env2) = eval env1 exp in 
                               (ValorBool ((b v1) && (b v2)), env2)
    ENot exp       -> let (v1,env1) = eval environment exp in 
                        (ValorBool (not (b v1)), env1)
    EStr str       -> (ValorStr str, environment)
    ETrue          -> (ValorBool True, environment)
    EFalse         -> (ValorBool False, environment)
    EInt n         -> (ValorInt n, environment)
    EVar id        -> (lookupDeepValue environment  id, environment)
    Call id lexp   -> let memoValue = lookupShallowFunctionCallEA environment (FCEA id evaluatedArguments) in
                        case memoValue of
                         OK v -> (v, environment)
                         Erro _ -> (newValue, (localEnv,globalEnv,((FCEA id evaluatedArguments),newValue):fccCtx'))
                       where ValorFun (Fun _ _ decls stms) = lookupShallowFunction environment id
                             parameterNames = (map (\(Dec _ id) -> id) decls) 
                             evaluatedArguments = map (\exp -> fst(eval environment exp)) lexp
                             paramBindings = zip parameterNames evaluatedArguments
                             nEnv@(_,_,fccCtx') = execute ([paramBindings],globalEnv,fcEnv) (SBlock stms)
                             newValue = lookupShallowValue nEnv (Ident "return")

data Valor = ValorInt Integer |  
             ValorBool Bool |  
             ValorStr  String | 
             ValorFun Function deriving Eq

i :: Valor -> Integer
i (ValorInt inte) = inte

s :: Valor -> String
s (ValorStr str) = str

b :: Valor -> Bool
b (ValorBool bo) = bo
               

initVal :: Type -> Valor
initVal Tbool = ValorBool False
initVal Tint  = ValorInt 0
initVal TStr  = ValorStr ""

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun (Fun tp _ decls _)) = printTree tp ++ "<-" ++ "(" ++ printTree decls ++ ")" 

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

-- Funcion Call with Evaluated Arguments
data FunCallEA = FCEA Ident [Valor] deriving (Eq,Show)

type Environment = ([RContext],RContext,FCContext)
type RContext = [(Ident,Valor)]
type FCContext = [(FunCallEA,Valor)]


push :: Environment -> Environment
push (sc,fnCtx,fcc) = ([]:sc,fnCtx,fcc)

pushB :: RContext -> Environment -> Environment
pushB typeBindings (sc,fnCtx,fcc) = (typeBindings:sc,fnCtx,fcc) 

 
pop :: Environment -> Environment
pop ((s:scs),fnCtx,fcc) = (scs,fnCtx,fcc)

lookupDeepValueA :: Environment -> Ident -> R Valor
lookupDeepValueA ([],fnCtx,fcc) id = Erro (printTree id ++ " nao esta no contexto. ")
lookupDeepValueA ((s:scs),fnCtx,fcc) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> OK val
                                            Erro _ -> lookupDeepValueA (scs,fnCtx,fcc) id


lookupDeepValue :: Environment -> Ident -> Valor
lookupDeepValue ((s:scs),fnCtx,fcc) id =  let r = lookupShallow s id in
                                         case r of
                                            OK val -> val
                                            Erro _ -> lookupDeepValue (scs,fnCtx,fcc) id

lookupShallowValue :: Environment -> Ident -> Valor   
lookupShallowValue  ((s:sc),_,_) id =  (\(OK val) -> val) (lookupShallow s id)
                                      
lookupShallowFunction :: Environment -> Ident -> Valor
lookupShallowFunction (_,fnCtx,_) id = (\(OK val) -> val) (lookupShallow fnCtx id)



lookupShallowFunctionCallEA :: Environment -> FunCallEA -> R Valor
lookupShallowFunctionCallEA (_,_,fcCtx) functionCallEA = (lookupShallowFC fcCtx functionCallEA)

lookupShallowFC :: FCContext -> FunCallEA -> R Valor
lookupShallowFC [] s = Erro (show s ++ " nao esta no contexto. ")
lookupShallowFC ((i,v):cs) s
   | i == s =  OK v
   | otherwise = lookupShallowFC cs s

lookupShallow :: RContext -> Ident -> R Valor
lookupShallow [] s = Erro (show s ++ " nao esta no contexto. ")
lookupShallow ((i,v):cs) s
   | i == s =  OK v
   | otherwise = lookupShallow cs s

updateShallowValue :: Environment -> Ident -> Valor -> Environment
updateShallowValue ([],fnCtx,fcc) id tp = ([[(id,tp)]],fnCtx,fcc)
updateShallowValue ((s:sc),fnCtx,fcc) id tp = ( (updateShallow s id tp):sc , fnCtx,fcc)   

updateDeepValue :: Environment -> Ident -> Valor -> Environment
updateDeepValue ([],fnCtx,fcc) id tp = ([[(id,tp)]],fnCtx,fcc)
updateDeepValue ((s:sc),fnCtx,fcc) id tp = let r = lookupShallow s id in 
                                           case r of
                                               OK _ -> ( (updateShallow s id tp):sc , fnCtx,fcc)
                                               Erro _ -> pushB s (updateDeepValue (sc,fnCtx,fcc) id tp)    
                                             
updateShallow :: RContext -> Ident -> Valor -> RContext
updateShallow [] s nv = [(s,nv)]
updateShallow ((i,v):cs) s nv
        | i == s = (i,nv):cs
        | otherwise = (i,v) : updateShallow cs s nv
 
updatecF :: Environment -> [Function] -> Environment
updatecF e [] = e
updatecF (sc,c,fcc) (f@(Fun tp id params stms):fs) = updatecF (sc, updateShallow c id (ValorFun f),fcc) fs
               
updateFCContext :: Environment -> FunCallEA -> Valor -> Environment
updateFCContext (localCtx,fcCtx,fccCtx) fcea newValue = (localCtx,fcCtx, 
                                                          (fcea, newValue):fccCtx)

