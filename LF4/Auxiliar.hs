module Auxiliar where
import Prelude

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
  (>>=) :: R a -> (a -> R b) -> R b
  (OK x) >>= f = f x
  (Erro s) >>= _ = Erro s

isError :: R a -> Bool
isError e = case e of
  OK _ -> False
  Erro _ -> True
