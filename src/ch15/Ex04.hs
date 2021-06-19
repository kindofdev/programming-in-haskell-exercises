module Ex04 where

fibs :: [Integer]
fibs = 0 : 1 : [x + y | (x,y) <- zip fibs (tail fibs)]
  
first10Fibs = take 10 fibs  