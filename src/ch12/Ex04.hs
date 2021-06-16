module Ex04 where

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
--  fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)
  

instance Applicative ZipList where
--  pure :: a -> ZipList a
  pure x = Z (repeat x)
  
--  (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b 
  Z gs <*> Z xs = Z (zipWith ($) gs xs )   