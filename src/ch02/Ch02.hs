module Ch2 where

-- ex2    

e1 = (2^3)*4
e2 = (2*3)+(4*5)
e3 = 2+(3*(4^5))  

-- ex3

n = a `div` length xs
    where 
      a = 10
      xs = [1,2,3,4,5]

-- ex4

last' xs = head (reverse xs)  

last'' xs = xs !! (length xs - 1)
  
last''' []     = error "empty list"
last''' [x]    = x   
last''' (_:xs) = last' xs 

-- ex5

init' :: [a] -> [a]
init' [] = error "empty list"
init' xs = reverse (tail (reverse xs))

init'' :: [a] -> [a]
init'' [] = error "empty list"
init'' xs = take (length xs - 1) xs 