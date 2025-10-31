module Interpreter where

import AbsLF
import AbsLFAux
import Prelude hiding (lookup)

executeP :: Program -> Valor
executeP (Prog fs) = eval (updatecF [] fs) (expMain fs)
  where
    expMain (f : xs)
      | getName f == Ident "main" = getExp f
      | otherwise = expMain xs

eval :: RContext -> Exp -> Valor
eval context x = case x of
  ECon exp0 exp -> ValorStr (s (eval context exp0) ++ s (eval context exp))
  EAdd exp0 exp -> ValorInt (i (eval context exp0) + i (eval context exp))
  ESub exp0 exp -> ValorInt (i (eval context exp0) - i (eval context exp))
  EMul exp0 exp -> ValorInt (i (eval context exp0) * i (eval context exp))
  EDiv exp0 exp -> ValorInt (i (eval context exp0) `div` i (eval context exp))
  EOr exp0 exp -> ValorBool (b (eval context exp0) || b (eval context exp))
  EAnd exp0 exp -> ValorBool (b (eval context exp0) && b (eval context exp))
  ENot exp -> ValorBool (not (b (eval context exp)))
  EStr str -> ValorStr str
  ETrue -> ValorBool True
  EFalse -> ValorBool False
  EInt n -> ValorInt n
  EVar id -> lookup context id
  EIf exp expT expE -> case eval context exp of
    ValorInt v ->
      if v /= 0
        then eval context expT
        else eval context expE
  ECall id lexp -> eval (paramBindings ++ contextFunctions) (getExp funDef)
    where
      (ValorFun funDef) = lookup context id
      parameters = getParams funDef
      paramBindings = zip parameters (map (eval context) lexp)
      contextFunctions =
        filter
          ( \(i, v) -> case v of
              ValorFun _ -> True
              _ -> False
          )
          context

-- *** @dica: nao altere o todo o codigo abaixo a partir daqui

data Valor
  = ValorInt
      { i :: Integer
      }
  | ValorFun
      { f :: Function
      }
  | ValorStr
      { s :: String
      }
  | ValorBool
      { b :: Bool
      }

instance Show Valor where
  show (ValorBool b) = show b
  show (ValorInt i) = show i
  show (ValorStr s) = s
  show (ValorFun f) = show f

type RContext = [(Decl, Valor)] -- Contexto de execução: associa Decl (tipo e id) a Valor

lookup :: RContext -> Ident -> Valor
lookup ((Dec t i, v) : cs) s
  | i == s = v
  | otherwise = lookup cs s

update :: RContext -> Decl -> Valor -> RContext --
update [] s v = [(s, v)]
update ((d, v) : cs) s nv
  | d == s = (d, nv) : cs -- Se encontrar o decl (tp,id), atualiza o valor
  | otherwise = (d, v) : update cs s nv -- Senão, continua procurando

updatecF :: RContext -> [Function] -> RContext
updatecF = foldl (\c f -> update c (getDecl f) (ValorFun f)) -- Percorre a lista de funções, atualizando o contexto com cada função e seu valor (ValorFun)
