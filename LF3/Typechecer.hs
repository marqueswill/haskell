{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
module Typechecer where

import AbsLF
import Prelude hiding (lookup)
import PrintLF

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

isError e = case e of
    OK _ -> False
    Erro _ -> True 


type TContext = [(Ident,Type)]

typeCheckP :: Program  -> [R TContext]
typeCheckP (Prog fs) = let nCtx = updatecF [] fs in
                          case nCtx of
                             OK ctx -> map (typeCheckF ctx) fs
                             Erro msg -> [Erro msg]
                                                  
typeCheckF ::  TContext -> Function -> R TContext    
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
                                        where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) decls
                                              functionTypes = filter (\(i,t) -> case t of 
                                                                                 TFun _  _ -> True 
                                                                                 _ -> False
                                                                      ) tc
                                    
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = let r = tinf tc exp in
                          case r of
                             OK tipo -> if (tipo == tp)
                                           then OK tc
                                           else Erro ("@typechecker: a expressao "++ printTree exp ++ " tem o tipo " ++ 
                                                     printTree tipo ++ " mas o tipo esperado eh "
                                                     ++ printTree tp)
                             Erro msg -> Erro msg  

tinf :: TContext -> Exp -> R Type
tinf tc x  =  case x of
    ECon exp0 exp  -> combChecks tc exp0 exp TStr
    EAdd exp0 exp  -> combChecks tc exp0 exp Tint
    ESub exp0 exp  -> combChecks tc exp0 exp Tint
    EMul exp0 exp  -> combChecks tc exp0 exp Tint
    EDiv exp0 exp  -> combChecks tc exp0 exp Tint
    EOr  exp0 exp  -> combChecks tc exp0 exp Tbool
    EAnd exp0 exp  -> combChecks tc exp0 exp Tbool
    ENot exp       -> let r = tke tc exp Tbool in 
                         case r of 
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    EStr str       -> OK TStr  
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookup tc id
    eIf@(EIf exp expT expE) -> let r = tke tc exp Tint in
                                 case r of
                                   OK _ -> let r2 = tinf tc expT  in 
                                             case r2 of
                                               OK tExpT -> let r3 = tinf tc expE in 
                                                            case r3 of
                                                              OK tExpE -> if tExpT == tExpE
                                                                             then OK tExpT
                                                                             else Erro ("tipos das expressoes do IF na expressao: " ++ printTree eIf)                                 
                                                              Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)                                                            
                                               Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)
                                   Erro msg -> Erro (msg ++ " na expressao: " ++ printTree eIf)
                                   
    -- TODO: 1) completar abaixo trocando undefined pelo retorno apropriado 
    -- TODO: 2) explicar o argumento de tinf abaixo
    ELambda params exp -> case (tinf (parameterTypeBindings ++ tc) exp) of  
                            OK tExp -> OK (TFun tExp paramTypes) -- Retorno um tipo TFun que a expressão lambda representa
                            Erro msg -> Erro msg
                           {- Extraio os pares (tipo, nome) da lista de parâmetros da função lambda para serem usados no contexto de tipos local da expressão lambda -}
                           where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) params
                                 paramTypes = map (\(Dec tp id) -> tp) params

    -- TODO: 1) completar abaixo trocando undefined pelo retorno apropriado
    -- TODO: 2) fazer as explicacoes necessarias
    ECall exp lexp  -> case (tinf tc exp) of  
                        OK (TFun tR pTypes) ->  if (length pTypes >= length lexp) then      -- TODO ==> Verifica se o número de argumentos é menor (aplicação parcial) ou igual (aplicação total) ao número de parâmetros da função
                                                    if (isThereError tksArgs /= []) then 
                                                      Erro " @typechecker: tipo incompativel entre argumento e parametro"
                                                    else 
                                                      if (length pTypes > length lexp)      -- TODO: O que isso testa? ==> Verifica se o número de parâmetro é maior que o de argumentos
                                                        then OK (TFun tR partialParamTypes) -- Se eu tenho menos argumnetos que parâmetros, eu retorna o tipo da função parcial que será gerada (aplicação parcial)
                                                        else OK tR                          -- Caso contrário, retorna o tipo da função (aplicação total)
                                                -- Se tiver mais argumentos que parâmetros lanço erro
                                                else Erro " @typechecker: mais argumentos que parametros" 

                                                where tksArgs = zipWith (tke tc) lexp pTypes
                                                      isThereError l = filter (==False) 
                                                                              (map (\e->(let r2 = e in  
                                                                                              case r2 of
                                                                                                OK _ -> True
                                                                                                Erro _ -> False)) l)

                                                      partialParamTypes = drop (length lexp) pTypes

                                                      {-  Exemplo: 
                                                          f :: Integer -> Integer -> Integer -> Integer
                                                          f a b c = a + b + c

                                                          f 2 vai gerar uma função 'g' onde
                                                          g :: Integer -> Integer -> Integer
                                                          g b c = 2 + b + c

                                                          Ou seja, o tipo de retorno é o mesmo que o tipo original da função, 
                                                          mas eu removo o tipo dos argumentos que já foram fornecidos.
                                                      -}
                        OK t -> Erro ("@typechecker: tipo deveria ser funcao em " ++ printTree exp ++ " tipo real: " ++ show t)
                        Erro msg -> Erro msg

    -- TODO: O que esta sendo testando abaixo ?
    {- Faz a inferência de tipos para cada uma das expressões presentes na composição de funções
       Se os tipos delas forem válidos, verifica se o tipo de retorno de exp2 é igual ao tipo dos argumentos de exp1
       Isto é, seja <f . g> uma composição de funções, é verificado se a imagem de 'g' está contida no domínio de 'f'
    -}              
    EComp exp1 exp2 -> case (tinf tc exp1, tinf tc exp2) of
                        (OK (TFun tprExp1 tpsExp1) , OK (TFun tprExp2 tpsExp2) )  ->
                          if ([tprExp2] == tpsExp1) then -- Verifica se o tipo de retorno da função 2 é igual ao tipo dos argumentos da função 1 
                            OK (TFun tprExp1 tpsExp2)    -- Se forem iguais, é possível aplicar a função composta. A função composta tem os argumentos da segunda função e tipo de retorno da primeira
                          else 
                            Erro "erro..."



-- *** nao altere o codigo abaixo ***
combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = let r = tke tc exp1 tp in
                                       case r of
                                          OK _ -> let r2 = tke tc exp2 tp in
                                                     case r2 of 
                                                         OK _ -> OK tp
                                                         Erro msg -> Erro msg
                                          Erro msg -> Erro msg 

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id,value):cs) key
   | id == key = OK value
   | otherwise = lookup cs key


updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id,tp)]
updateTC ((id,tp):idTps) idN tpN 
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = let r = (updateTC idTps idN tpN) in       
                  case r of 
                    OK restOK -> OK ((id,tp) : restOK)    
                    Erro msg -> Erro msg 

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _ )-> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _):fs) = let r = updateTC tc nomeF (getFunctionType f) in
                                                   case r of 
                                                     OK tcNew -> updatecF tcNew fs
                                                     Erro msg -> Erro msg


