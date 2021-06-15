module Ch6 where

import Prelude hiding ((^), and, concat, replicate, (!!), elem)
  
-- ex1

fac :: Int -> Int
fac 0             = 1
fac n | n > 0     = n * fac (n-1)
      | otherwise = error "negative number"


-- ex2

sumdown :: Int -> Int
sumdown 0 = 0  
sumdown n = n + sumdown (n-1) 


-- ex3

(^) :: Int -> Int -> Int
n ^ 0 = 1 
n ^ e = n * (n ^ (e-1))

--2 ^ 3
--2 * (2 ^ 2)
--2 * (2 * (2 ^ 1))
--2 * (2 * (2 * (2 ^ 0)))
--2 * (2 * (2 * 1))
--8


-- ex4

euclid :: Int -> Int -> Int
euclid x y | x == y = x
           | x < y  = euclid x (y-x)
           | x > y  = euclid (x-y) y



-- ex5

--length [1,2,3]
--1 + length [2,3]
--1 + 1 + length [3]
--1 + 1 + 1 + length []
--1 + 1 + 1 + 0
--3

--drop 3 [1,2,3,4,5]
--drop 2 [2,3,4,5]
--drop 1 [3,4,5]
--drop 0 [4,5]
--[4,5]

--init [1,2,3]
--1 : init [2,3]
--1 : 2 : init [3]
--1 : 2 : []


-- ex6

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

concat :: [[a]] -> [a]
concat []       = []
concat (xs:xss) = xs ++ concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x

(!!) :: [a] -> Int -> a
_      !! n | n < 0 = error "negative index"
[]     !! _         = error "index out of range"
(x:_)  !! 0         = x
(_:xs) !! n         = xs !! (n-1)

elem :: Eq a => a -> [a] -> Bool
elem _ []      = False
elem x' (x:xs) | x' == x   = True
               | otherwise = elem x' xs


-- ex7

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                      = ys
merge xs []                      = xs
merge (x:xs) (y:ys) | x <= y     = x : merge xs (y:ys)
                    | otherwise  = y : merge (x:xs) ys


-- ex8

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort xs1) (msort xs2)
            where (xs1, xs2) = halve xs

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs


-- ex9

sum' :: Num a => [a] -> a
sum' []     = 0 
sum' (x:xs) = x + sum' xs


take' :: Int -> [a] -> [a]
take' 0 xs     = []  
take' n []     = [] 
take' n (x:xs) = x : take' (n-1) xs  


last' :: [a] -> a
last' [x]    = x 
last' (_:xs) = last' xs 


