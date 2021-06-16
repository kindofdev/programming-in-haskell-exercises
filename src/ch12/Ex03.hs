module Ex03 where

instance Applicative ((->) a) where
--  pure :: b -> (a -> b)
  pure = const
  
--  <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
  (<*>) g h = \x -> g x (h x)   
  