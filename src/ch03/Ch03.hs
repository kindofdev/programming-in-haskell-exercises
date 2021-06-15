module Ch1 where

-- ex1

x1 = ['a', 'b', 'c'] :: [Char]

x2 = ('a', 'b', 'c') :: (Char, Char, Char)

x3 = [(False, '0'), (True, '1')] :: [(Bool, Char)]

x4 = ([False, True], ['0', '1']) :: ([Bool], [Char])

x5 = [tail, init, reverse] :: [[a] -> [a]]


-- ex2

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1, 2], [3, 4]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x


-- ex3

second :: [a] -> a
second xs = head (tail xs)

swap :: (a, b) -> (b, a)
swap (x,y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a 
twice f x = f (f x)


-- ex4 

{- 
NA 
-}


-- ex5

{- 
It's not possible equals two functions because we should test the equality of the output for every possible input of
the two functions. Normally, the input domain is infinite.      
-}










