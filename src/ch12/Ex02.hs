module Ex02 where

instance Functor ((->) a) where
--  fmap :: (b -> c) -> (a -> b) -> (a -> c) 
  fmap = (.)