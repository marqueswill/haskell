module AbsLFAux where

import AbsLF

getType :: Function -> Type
getType (Fun tp name params exp) = tp

getDecl :: Function -> Decl
getDecl (Fun tp name params exp) = Dec tp name

getName :: Function -> Ident
getName (Fun tp name params exp) = name

getParams :: Function -> [Decl]
getParams (Fun tp name params exp) = params

getExp :: Function -> Exp
getExp (Fun tp name params exp) = exp