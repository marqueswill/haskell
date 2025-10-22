{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
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

-- Retorna a lista com o tipo e os nomes dos parâmetros da função lambda
getParamsTypesL :: Exp -> [Decl]
getParamsTypesL (ELambda params _) =  params

-- Retorna o nome dos parâmetros como uma lista de expressões
getParamsExpL :: Exp -> [Exp]
getParamsExpL (ELambda params _) = map (\(Dec _ param) -> (EVar param)) params

getExpL :: Exp -> Exp 
getExpL (ELambda _ exp) = exp 
