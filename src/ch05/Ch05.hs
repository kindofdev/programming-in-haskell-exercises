module Ch5 where

import Data.Char

-- ex1

squaresSum :: Int
squaresSum = sum [x^2 | x <- [1..100]]


-- ex2

grid :: Int -> Int -> [(Int,Int)]
grid m n = [(x, y) |x <- [0..m], y <- [0..n]]


-- ex3

square :: Int -> [(Int,Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]


-- ex4

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]


-- ex5

pyths :: Int -> [(Int,Int,Int)]
pyths l = [(x,y,z) | x <- [1..l]
                   , y <- [1..l]
                   , z <- [1..l]
                   , x^2 + y^2 == z^2]


-- ex6

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum $ init $ factors x) == x]



-- ex7

foo = [(x,y) | x <- [1,2], y <- [3,4]]

foo' = concat [[(x, y) | y <- [3, 4]] | x <- [1, 2]]

result = foo == foo'


-- ex8

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x $ zip xs [0..] 


-- ex9

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x*y | (x,y) <- zip xs ys]


-- ex10

encode' :: Int -> String -> String
encode' n xs = [shift' n x | x <- xs]

shift' :: Int -> Char -> Char
shift' n c | isLower c || isUpper c = int2let ((let2int c + n) `mod` 26)
           | otherwise = c



----- chapter library ------

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = filter prime [2..n]

primes' :: Int -> [Int]
primes' n = [x | x <- [2..n], prime x]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k' == k]

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = all (\(v1,v2) -> v1 <= v2) (pairs xs)

sorted' :: Ord a => [a] -> Bool
sorted' xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x | x' <- xs, x == x']

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)
