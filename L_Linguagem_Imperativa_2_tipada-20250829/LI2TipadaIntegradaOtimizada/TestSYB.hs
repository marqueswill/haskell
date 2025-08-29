{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics

data Company =  C [Dept]  deriving (Data,Show,Eq)
data Dept =     D Name Manager [SubUnit] deriving (Data,Show,Eq)
data SubUnit =  PU Employee | 
                DU Dept deriving (Data,Show,Eq)
data Employee = E Person Salary deriving (Data,Show,Eq)
data Person =   P Name Address deriving (Data,Show,Eq)
data Salary =   S Float deriving (Data,Show,Eq)
type Manager =  Employee
type Name =     String
type Address =  String

-- exemplo de uma empresa do tipo Company 
genCom :: Company
genCom = C [D "Research" ralf [PU joost, PU marlow], 
            D "Strategy" blair []]
ralf, joost, marlow, blair :: Employee
ralf = E (P "Ralf" "Amsterdam") (S 8000)
joost = E (P "Joost" "Amsterdam") (S 1000)
marlow = E (P "Marlow" "Cambridge") (S 2000)
blair = E (P "Blair" "London") (S 100000)

{- funcao increase que aumenta o salario da uma empresa
   percorrendo explicitamente a estrutura da empresa
   o programador deve escrever esse codigo -}

increase k (C ds) = C (map (incD k) ds)
incD :: Float -> Dept -> Dept
incD k (D nm mgr us) = D nm (incE k mgr) (map (incU k) us)
incU :: Float -> SubUnit -> SubUnit
incU k (PU e) = PU (incE k e)
incU k (DU d) = DU (incD k d)
incE :: Float -> Employee -> Employee
incE k (E p s) = E p (incS k s)
incS :: Float -> Salary -> Salary
incS k (S s) = S (s * (1+k))


{- funcao increaseSYB que aumenta o salario da uma empresa
   percorrendo implicitamente a estrutura da empresa
   com o SYB. O programador nao escreve o codigo de percorrer
   a estrutura da empresa
-}

increaseSYB :: Float -> Company -> Company
increaseSYB k = everywhere (mkT (incS k))

-- o valor de testCaseIncrease deve ser true
testCaseIncrease = increase 0.1 genCom == increaseSYB 0.1 genCom

--------------------------------------------------------------------


{- funcao salaryBill que calcula a folha de pagamento de uma empresa
   percorrendo explicitamente a estrutura da empresa.
   O programador deve escrever esse codigo de percorrimento da estrutura -}

salaryBill :: Company -> Float
salaryBill (C depts) = foldl (+) 0 (map salaryBillD depts)
salaryBillD (D _ mgr subs) = foldl (+) 0 ([salaryBillE mgr] ++ (map salaryBillSU subs))
salaryBillE (E _ (S salary)) = salary
salaryBillSU (PU employee) = salaryBillE employee
salaryBillSU (DU dept) = salaryBillD dept 




{- funcao salaryBillSYB que calcula a folha de pagamento de uma empresa
   percorrendo implicitamente a estrutura da empresa
   com o SYB. O programador nao escreve o codigo de percorrer
   a estrutura da empresa -}

salaryBillSYB :: Company -> Float
salaryBillSYB = everything (+) (0 `mkQ` billS)
billS :: Salary -> Float
billS (S f) = f

-- o valor de testCaseSalaryBill deve ser true
testCaseSalaryBill = salaryBill genCom == salaryBillSYB genCom



