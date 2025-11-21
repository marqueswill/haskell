module Optimizer where

import AbsLF
import Interpreter

optimizeP :: Program -> Program
optimizeP (Prog fs) = Prog (map optimizeF fs)

optimizeF :: Function -> Function
optimizeF (Fun tR id decls exp) = Fun tR id decls (optimizeE exp)

optimizeE :: Exp -> Exp
optimizeE exp = case exp of
  EStr str -> EStr str
  ETrue -> ETrue
  EFalse -> EFalse
  EInt n -> EInt n
  EVar id -> EVar id
  ENot exp ->
    let optExp = optimizeE exp
        optENot = ENot optExp
     in if isLiteral optExp
          then
            let (v1, env) = eval ([], []) optENot
             in wrapValueExpression v1
          else optENot
  ECon exp0 exp -> combOptimize ECon exp0 exp
  EAdd exp0 exp -> combOptimize EAdd exp0 exp
  ESub exp0 exp -> combOptimize ESub exp0 exp
  EMul exp0 exp -> combOptimize EMul exp0 exp
  EDiv exp0 exp -> combOptimize EDiv exp0 exp
  EOr exp0 exp -> combOptimize EOr exp0 exp
  EAnd exp0 exp -> combOptimize EAnd exp0 exp
  ECall id lexp -> ECall id (map optimizeE lexp)
  EIf exp expT expE ->
    let optExp = optimizeE exp
        optThen = optimizeE expT
        optElse = optimizeE expE
        optEIf = EIf optExp optThen optElse
     in case optExp of
          EInt vExpIf ->
            if vExpIf == 0
              then optElse
              else optThen
          _ -> optEIf

isLiteral :: Exp -> Bool
isLiteral exp = case exp of
  EStr _ -> True
  ETrue -> True
  EFalse -> True
  EInt _ -> True
  _ -> False

wrapValueExpression :: Valor -> Exp
wrapValueExpression (ValorInt i) = EInt i
wrapValueExpression (ValorStr s) = EStr s
wrapValueExpression (ValorBool True) = ETrue
wrapValueExpression (ValorBool False) = EFalse

combOptimize :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Exp
combOptimize expBinConst exp0 exp1 =
  let optExp0 = optimizeE exp0
      optExp1 = optimizeE exp1
      optBinExp = expBinConst optExp0 optExp1
   in if isLiteral optExp0 && isLiteral optExp1
        then
          let (v1, env1) = eval ([], []) optBinExp
           in wrapValueExpression v1
        else optBinExp
