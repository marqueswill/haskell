fibo :: Int -> Int
fibo 0  = 1
fibo 1 = 1
fibo n
     | n > 1 = fibo (n-1) + fibo (n-2)

{- 
int fibo (int n) {
 int r;
 if (n)
   then if (n-1)
          then return (fibo(n-1) + fibo(n-2));
          else return 1;
   else return 1;
 }
-}