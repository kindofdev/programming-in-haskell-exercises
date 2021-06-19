module Ex06 where

sqroot :: Double -> Double
sqroot n = snd . head $ dropWhile p (zip as (tail as))
           where as = iterate next 1.0
                 next a = (a + n/a) / 2
                 p (x,x') = abs (x - x') >= precision 

precision :: Double
precision = 0.00001
