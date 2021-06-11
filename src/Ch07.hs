module Ch7 where
import Data.Char

-- ex1

fm :: (a -> Bool) -> (a -> b) -> [a] -> [b]
fm p f = map f . filter p
--fm p f xs = [f x | x <- xs, p x]


-- ex2

all' :: (a -> Bool) -> [a] -> Bool
all' p = foldr (\x b -> p x && b) True
--all' p = and . map p

any' :: (a -> Bool) -> [a] -> Bool
any' p = foldr (\x b -> p x || b) False
--any' p = or . map p

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ []                 = []
takeWhile' p (x:xs) | p x       = x : takeWhile' p xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ []                 = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs


-- ex3

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f x : xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x xs -> if p x then x : xs else xs) []


-- ex4

dec2int :: [Int] -> Int
dec2int ns = foldl (\acc (n,i) -> n * 10^i + acc) 0 (zip (reverse ns) [0..])
--dec2int = foldl (\x y -> 10*x + y) 0

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f = \x -> (\y -> f (x, y))

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y


-- ex6

unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

type Bit = Int

chop8' :: [Bit] -> [[Bit]]
chop8' = unfold null (take 8) (drop 8)

map'' :: (a -> b) -> [a] -> [b]
map'' f = unfold null (f . head) tail

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfold (const False) id f


-- ex7

bin2int :: [Bit] -> Int
bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 $ bits ++ repeat 0

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

decode :: [Bit] -> String
decode = map (chr . bin2int) . chopn 8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode

--------

chopn :: Int -> [Bit] -> [[Bit]]
chopn n = unfold null (take n) (drop n)

countOnes :: [Bit] -> Int
countOnes = length . filter (==1)

addParityBit :: [Bit] -> [Bit]
addParityBit bits | even $ countOnes bits = 0 : bits
                  | otherwise             = 1 : bits

checkParity :: [Bit] -> [Bit]
checkParity bits | even (countOnes bits) = bits
                 | otherwise             = error "parity error"

removeParityBit :: [Bit] -> [[Bit]]
removeParityBit bits =  map (drop 1 . checkParity) (chopn 9 bits)

encode' :: String -> [Bit]
encode' = concat . map (addParityBit . make8 . int2bin . ord)

decode' :: [Bit] -> String
decode' = map (chr . bin2int) . removeParityBit

transmit' :: ([Bit] -> [Bit]) -> String -> String
transmit' channel = decode' . channel . encode'

test7 :: String
test7 = transmit' channel "testing exercise 7"

-- ex8

failedChannel :: [Bit] -> [Bit]
failedChannel = tail

test8 :: String
test8 = transmit' failedChannel "testing exercise 8"


-- ex9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g xs = map (\(f', x) -> f' x) ps
  where fs = cycle [f, g]
        ps = zip fs xs


-- ex10

luhnDouble :: Int -> Int
luhnDouble n | doubled >= 9 = doubled - 9
             | otherwise    = doubled
  where doubled = 2*n

luhn :: [Int] -> Bool
luhn ns = total `mod` 10 == 0 
  where total = sum $ altMap luhnDouble id ns 

