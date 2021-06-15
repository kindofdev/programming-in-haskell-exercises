module Ch1 where

-- ex3 

product' []     = 1
product' (n:ns) = n * product' ns  


-- ex4

qsortR :: Ord a => [a] -> [a]
qsortR []     = []
qsortR (x:xs) = qsortR larger ++ [x] ++ qsortR smaller
  where smaller = [a | a <- xs, a < x] 
        larger  = [b | b <- xs, b > x]
        
-- ex5

{- It removes duplicates -}


