module Ex06 where

instance Monad ((->) a) where
--  return :: b -> (a -> b) 
  return = const

--  (>>=) (b -> (a -> c)) -> (a -> b) -> (a -> c) 
  g >>= h = \x -> g (h x) x
  
  
  