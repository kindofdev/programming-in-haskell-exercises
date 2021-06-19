module Ex05 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _            = Leaf
take' _ Leaf         = Leaf
take' n (Node l x r) = Node (take' (n-1) l) x (take' (n-1) r)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat'

