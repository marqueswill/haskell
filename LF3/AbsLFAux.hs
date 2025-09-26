module AbsLFAux where

import AbsLF

getName :: Function -> Ident
getName (Fun _ name _ _) = name

getParams :: Function -> [Decl]
getParams (Fun _ _ decls _) = decls

getExp :: Function -> Exp 
getExp (Fun _ _ _ exp) = exp 

getParamsL :: Exp -> [Ident]
getParamsL (ELambda params _) = map (\(Dec _ param)-> param) params

getParamsTypesL :: Exp -> [Decl]
getParamsTypesL (ELambda params _) =  params

getParamsExpL :: Exp -> [Exp]
getParamsExpL (ELambda params _) = map (\(Dec _ param)-> (EVar param)) params

getExpL :: Exp -> Exp 
getExpL (ELambda _ exp) = exp 
