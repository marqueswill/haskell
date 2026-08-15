module Typechecker where

import AbsLF
import Auxiliar
import PrintLF
import Prelude hiding (lookup)

type TContext = [(Ident, Type)]

typeCheckP :: Program -> [R TContext]
typeCheckP (Prog fs) =
  let nCtx = updatecF [] fs
   in case nCtx of
        OK ctx -> map (typeCheckF ctx) fs
        Erro msg -> [Erro msg]

typeCheckF :: TContext -> Function -> R TContext
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR -- O contexto de tipos para função é criado para cada chamada do typeCheckF, assim não há conflito de argumentos com as demais funções
  where
    parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls -- Retorna uma lista dos parâmetros e seus tipos
    functionTypes =
      filter
        ( \(i, t) -> case t of -- Retorna uma lista das funções e seus tipos
            TFun _ _ -> True
            _ -> False
        )
        tc

tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = do
  tipo <- tinf tc exp
  if tipo == tp
    then OK tc
    else
      Erro
        ( "@typechecker: a expressao "
            ++ printTree exp
            ++ " tem o tipo "
            ++ printTree tipo
            ++ " mas o tipo esperado eh "
            ++ printTree tp
        )

tinf :: TContext -> Exp -> R Type
tinf tc x = case x of
  ECon exp0 exp -> combChecks tc exp0 exp TStr
  EAdd exp0 exp -> combChecks tc exp0 exp Tint
  ESub exp0 exp -> combChecks tc exp0 exp Tint
  EMul exp0 exp -> combChecks tc exp0 exp Tint
  EDiv exp0 exp -> combChecks tc exp0 exp Tint
  EOr exp0 exp -> combChecks tc exp0 exp Tbool
  EAnd exp0 exp -> combChecks tc exp0 exp Tbool
  ENot exp -> do
    _ <- tke tc exp Tbool
    OK Tbool
  EStr str -> OK TStr
  ETrue -> OK Tbool
  EFalse -> OK Tbool
  EInt n -> OK Tint
  EVar id -> lookup tc id
  eIf@(EIf exp expT expE) -> do
    _ <- tke tc exp Tint
    tExpT <- tinf tc expT
    tExpE <- tinf tc expE
    if tExpT == tExpE
      then OK tExpT
      else Erro ("tipos das expressoes do IF na expressao: " ++ printTree eIf)
  ECall id lexp ->
    case lookup tc id of
      Erro msg -> Erro msg
      OK (TFun tR pTypes)
        | length pTypes /= length lexp ->
            Erro " @typechecker: tamanhos diferentes de lista de argumentos e parametros"
        | any isErro tksArgs ->
            Erro " @typechecker: chamada de funcao invalida"
        | otherwise -> OK tR
        where
          tksArgs = zipWith (tke tc) lexp pTypes

          isErro (Erro _) = True
          isErro _ = False

combChecks :: TContext -> Exp -> Exp -> Type -> R Type
combChecks tc exp1 exp2 tp = do
  _ <- tke tc exp1 tp
  _ <- tke tc exp2 tp
  OK tp

lookup :: TContext -> Ident -> R Type
lookup [] id = Erro ("@typechecker: " ++ printTree id ++ " nao esta no contexto. ")
lookup ((id, value) : cs) key
  | id == key = OK value
  | otherwise = lookup cs key

updateTC :: TContext -> Ident -> Type -> R TContext
updateTC [] id tp = OK [(id, tp)]
updateTC ((id, tp) : idTps) idN tpN
  | id == idN = Erro ("@typechecker: identificador" ++ printTree id ++ " nao pode ter mais de um tipo")
  | otherwise =
      let r = updateTC idTps idN tpN
       in case r of
            OK restOK -> OK ((id, tp) : restOK)
            Erro msg -> Erro msg

getFunctionType :: Function -> Type
getFunctionType (Fun tipoRetorno _ decls _) = TFun tipoRetorno (map (\(Dec tp _) -> tp) decls)

updatecF :: TContext -> [Function] -> R TContext
updatecF tc [] = OK tc
updatecF tc (f@(Fun _ nomeF _ _) : fs) =
  let r = updateTC tc nomeF (getFunctionType f)
   in case r of
        OK tcNew -> updatecF tcNew fs
        Erro msg -> Erro msg