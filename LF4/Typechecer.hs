{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant ==" #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Typechecer where

import AbsLF
import PrintLF
import Prelude hiding (lookup)

data R a
  = OK a
  | Erro String
  deriving (Eq, Ord, Show, Read)

instance Functor R where
  fmap :: (a -> b) -> R a -> R b
  fmap f (OK x) = OK (f x)
  fmap _ (Erro s) = Erro s

instance Applicative R where
  pure :: a -> R a
  pure = OK

  (<*>) :: R (a -> b) -> R a -> R b
  (OK f) <*> (OK x) = OK (f x)
  (Erro s) <*> _ = Erro s
  _ <*> (Erro s) = Erro s

instance Monad R where
  return :: a -> R a
  return = OK

  (>>=) :: R a -> (a -> R b) -> R b
  (OK x) >>= f = f x
  (Erro s) >>= _ = Erro s

isError :: R a -> Bool
isError e = case e of
  OK _ -> False
  Erro _ -> True

type TContext = [(Ident, Type)]

typeCheckP :: Program -> [R TContext]
typeCheckP (Prog fs) =
  let nCtx = updatecF [] fs
   in case nCtx of
        OK ctx -> map (typeCheckF ctx) fs
        Erro msg -> [Erro msg]

typeCheckF :: TContext -> Function -> R TContext
typeCheckF tc (Fun tR _ decls exp) = tke (parameterTypeBindings ++ functionTypes) exp tR
  where
    parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) decls
    functionTypes =
      filter
        ( \(i, t) -> case t of
            TFun _ _ -> True
            _ -> False
        )
        tc

tke :: TContext -> Exp -> Type -> R TContext
tke tc exp tp = do
  tipo <- tinf tc exp
  if (tipo == tp)
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
  ELambda params exp -> case (tinf (parameterTypeBindings ++ tc) exp) of
    OK tExp -> OK (TFun tExp paramTypes)
    Erro msg -> Erro msg
    where
      parameterTypeBindings = map (\(Dec tp id) -> (id, tp)) params
      paramTypes = map (\(Dec tp id) -> tp) params
  ECall exp lexp -> case (tinf tc exp) of
    OK (TFun tR pTypes) ->
      if (length pTypes >= length lexp)
        then
          if (isThereError tksArgs /= [])
            then
              Erro " @typechecker: tipo incompativel entre argumento e parametro"
            else
              if (length pTypes > length lexp)
                then
                  OK (TFun tR partialParamTypes)
                else
                  OK tR
        else
          Erro " @typechecker: mais argumentos que parametros"
      where
        tksArgs = zipWith (tke tc) lexp pTypes
        isThereError l =
          filter
            (== False)
            ( map
                ( \e ->
                    ( let r2 = e
                       in case r2 of
                            OK _ -> True
                            Erro _ -> False
                    )
                )
                l
            )

        partialParamTypes = drop (length lexp) pTypes
    OK t -> Erro ("@typechecker: tipo deveria ser funcao em " ++ printTree exp ++ " tipo real: " ++ show t)
    Erro msg -> Erro msg
  EComp exp1 exp2 -> case (tinf tc exp1, tinf tc exp2) of
    (OK (TFun tprExp1 tpsExp1), OK (TFun tprExp2 tpsExp2)) ->
      if ([tprExp2] == tpsExp1)
        then
          OK (TFun tprExp1 tpsExp2)
        else
          Erro "erro..."

-- *** nao altere o codigo abaixo ***

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
      let r = (updateTC idTps idN tpN)
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
