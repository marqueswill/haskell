module Typechecker where

import AbsLF
import Prelude hiding (lookup)
import PrintLF

data R a = OK a | Erro String                                   
         deriving (Eq, Ord, Show, Read)

isError e = case e of
    OK _ -> False
    Erro _ -> True 


type TContext = [(Ident,Type)]

{-
int main ()
{
  fat (5)
}
int fat (int n)
{
  if (n) then n * fat (n - 1) else 1
}
-}

test1 = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fat") [EInt 5]),Fun Tint (Ident "fat") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]
test2 = Prog [Fun Tint (Ident "main") [] (ECall (Ident "fat") [EAdd (EInt 5) (EInt 1)]),Fun Tint (Ident "fat") [Dec Tint (Ident "n")] (EIf (EVar (Ident "n")) (EMul (EVar (Ident "n")) (ECall (Ident "fat") [ESub (EVar (Ident "n")) (EInt 1)])) (EInt 1))]

typeCheckP :: Program  -> [R TContext] -- Retorna uma lista de contextos das funções, cada um fala o tipo da função e o tipo dos seus parametros e args
typeCheckP (Prog fs) = let nCtx = updatecF [] fs in
                          case nCtx of
                             OK ctx -> map (typeCheckF ctx) fs
                             Erro msg -> [Erro msg]

{- TODO: na definição de "typeCheckF" abaixo,substitua "undefined" 
         pelo argumento relevante -}
-- Define o escopo para verificação da expressão que abstrai a função. O contexto da função tem os argumentos usados na expressão, e outras funções que existem nela.                           
typeCheckF ::  TContext -> Function -> R TContext    
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR                  -- O contexto de tipos para função é criado para cada chamada do typeCheckF, assim não há conflito de argumentos com as demais funções
                                        where parameterTypeBindings = map (\(Dec tp id) -> (id,tp)) decls -- Retorna uma lista dos parâmetros e seus tipos
                                              functionTypes = filter (\(i,t) -> case t of                 -- Retorna uma lista das funções e seus tipos
                                                                                 TFun _  _ -> True 
                                                                                 _ -> False
                                                                      ) tc 

{- "tke" é uma função que dado, um contexto de tipos, uma expressão, e um tipo,
   verifica se essa expressão tem esse tipo ou retorna um erro se a expressão- 
   for mal tipada -}                            
-- Ela chama a função tinf, que é a que faz a inferência de tipo da função em si. Tke funciona como um "toplevel" ou um trycatch     
tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = let r = tinf tc exp in  -- Faz a inferência de tipo da expressão passada
                          case r of
                             OK tipo -> if (tipo == tp) -- Compara o tipo inferido com o tipo esperado
                                           then OK tc   -- Se tudo tá ok, o contexto até aqui também está ok.
                                           else Erro ("@typechecker: a expressao "++ printTree exp ++ " tem tipo " ++ 
                                                     printTree tipo ++ " mas o tipo esperado eh "
                                                     ++ printTree tp)
                             Erro msg -> Erro msg  


{- "tinf" é uma função que dado, um contexto de tipos e uma expressão, retorna
   o tipo dessa expressão ou um erro se a expressão for mal tipada -}
-- Ele é tal qual o "eval", com chamada recursiva usando o combChecks. Se tudo der certo, ela retorna o tipo que a função retorna          
tinf :: TContext -> Exp -> R Type
tinf tc x  =  case x of
    -- TIPOS BINÁRIOS
    ECon exp0 exp  -> combChecks tc exp0 exp TStr -- Verificação de erro feito em combChecks
    EAdd exp0 exp  -> combChecks tc exp0 exp Tint
    ESub exp0 exp  -> combChecks tc exp0 exp Tint
    EMul exp0 exp  -> combChecks tc exp0 exp Tint
    EDiv exp0 exp  -> combChecks tc exp0 exp Tint
    EOr  exp0 exp  -> combChecks tc exp0 exp Tbool
    EAnd exp0 exp  -> combChecks tc exp0 exp Tbool
  
    ENot exp       -> let r = tke tc exp Tbool in -- Verifica se o tipo é booleano
                         case r of                -- Verificação de erro
                             OK _ -> OK Tbool
                             Erro msg -> Erro msg
    --  TIPOS LITERAIS
    EStr str       -> OK TStr
    ETrue          -> OK Tbool 
    EFalse         -> OK Tbool  
    EInt n         -> OK Tint  
    EVar id        -> lookup tc id

{- TODO: implemente a checagem de tipo para o "EIf" abaixo:
   "exp" deve ser inteiro (Tint), e os tipos de "expT" e "expE" devem ser iguais.
   @dica: estude a estrutura da checagem de tipo do "SIf" na LI2Tipada. 
-}  
    eIf@(EIf exp expT expE) -> let r = tke tc exp Tint in
                                  case r of
                                     OK _ -> let tp1 = tinf tc expT in
                                      case tp1 of 
                                        OK _ -> let tp2 = tinf tc expE in
                                          case tp2 of
                                            OK _ -> if tp1 == tp2 then tp1
                                                    else Erro "Tipos do corpo da expressão eIf são diferentes"
                                            Erro msg -> Erro (msg ++ " na expressão: " ++ printTree expE)
                                        Erro msg -> Erro (msg ++ " na expressão: " ++ printTree expT)                                   
                                     Erro msg -> Erro (msg ++ " na expressão: " ++ printTree eIf)

-- TODO: sobre "ECall" abaixo, a lógica permanece a mesma em relação a LI2Tipada ? Por que? 
-- Compara a quantidade de parâmetros e argumentos fornecidos para função. 
-- Se baterem, verifica se os argumentos estão de acordo com o tipo dos paramentros 
    ECall id lexp   -> let r = lookup tc id in -- Procura a função no contexto de tipos
                        case r of 
                           OK (TFun tR pTypes) -> if (length pTypes == length lexp) -- Se encontrar a função e seu tipo está correto, compara a quant de parametros e argumentos
                                                    then 
                                                      if (isThereError tksArgs /= [])
                                                        then Erro " @typechecker: chamada de funcao invalida"
                                                        else OK tR
                                                      else Erro " @typechecker: tamanhos diferentes de lista de argumentos e parametros"

                                                      where tksArgs = zipWith (tke tc) lexp pTypes -- Checa os tipos dos argumentos e parâmetros
                                                            isThereError l = filter (==False)  
                                                                                    (map (\e->(let r2 = e in  
                                                                                                case r2 of
                                                                                                  OK _ -> True
                                                                                                  Erro _ -> False)) 
                                                                                         l)
                           Erro msg -> Erro msg


{- *** @dica: nao altere o codigo abaixo até o final do arquivo*** 
              mas saiba explicar o que ele faz
-}

-- Faz chamada recursiva para checagem de tipo de cada lado da expressão
-- Chama tke para cada subexpressão. Ambas devem ser do mesmo tipo para que não ocorra erro             
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

-- Atualiza o contexto de tipos fornecido. Se a função não estiver no contexto, ela e seu tipo são adicionados. 
-- Se ela estiver, a função é atualizada e o novo tipo verificado. O novo tipo deve bater com tipo fornecido
updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id,tp)]
updateTC ((id,tp):idTps) idN tpN 
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise = let r = (updateTC idTps idN tpN) in -- Primeiro chama recursivamente (map)
                  case r of 
                    OK restOK -> OK ((id,tp) : restOK)    
                    Erro msg -> Erro msg 

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _ )-> tp) decls)

-- Cria o contexto de tipo, que percorre as funções do programa recursivamente, adicionando a função e o tipo dela ao CT (updateTC)
updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _):fs) = let r = updateTC tc nomeF (getFunctionType f) in
                                                   case r of 
                                                     OK tcNew -> updatecF tcNew fs
                                                     Erro msg -> Erro msg
                                                     
                                                     
                                                     


