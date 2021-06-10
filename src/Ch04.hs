module Ch4 where

-- ex1

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = length xs `div` 2


-- ex2

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:_) = x


-- ex3

safeTail :: [a] -> [a]
safeTail xs = if null xs
              then []
              else tail xs

safeTail' :: [a] -> [a]
safeTail' xs | null xs   = []
             | otherwise = tail xs

safeTail'' :: [a] -> [a]
safeTail'' []     = []
safeTail'' (_:xs) = xs


-- ex4

--(||) :: Bool -> Bool -> Bool
--False || False = False
--False || True  = True
--True  || False = True
--True  || True  = True

--(||) :: Bool -> Bool -> Bool
--False || False = False
--_     || _     = True

--(||) :: Bool -> Bool -> Bool
--False || b = b
--True  || _ = True


(||) :: Bool -> Bool -> Bool
b || c | b == c    = b
       | otherwise = True


-- ex5

--(&&) :: Bool -> Bool -> Bool
--x && y = if x == True
--         then if y == True
--              then True
--              else False
--         else False


-- ex6

(&&) :: Bool -> Bool -> Bool
x && y = if x == True
         then y
         else False


-- ex7

mult :: Int -> Int -> Int -> Int
mult = \x -> (\y -> (\z -> x*y*z))


-- ex8

luhnDouble :: Int -> Int
luhnDouble n | doubled >= 9 = doubled - 9
             | otherwise    = doubled
  where doubled = 2*n


luhn :: Int -> Int -> Int -> Int -> Bool
luhn x1 x2 x3 x4 = total `mod` 10 == 0
  where total = x4 + luhnDouble x3 + x2 + luhnDouble x1 

