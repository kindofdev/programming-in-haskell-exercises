module Ch9 where

data Op = Add | Sub | Mul | Div

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) =  values l ++ values r

eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs)| (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []     = []
exprs [n]    = [Val n]
exprs ns = [e | (xs, ys) <- split ns,
                l        <- exprs xs,
                r        <- exprs ys,
                e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- ex1

choices' :: [a] -> [[a]]
choices' xs = [zs | ys <- subs xs, zs <- perms ys]


-- ex2

removeFstOcu :: Eq a => a -> [a] -> [a]
removeFstOcu _ []                  = []
removeFstOcu x (y:ys) | x == y     = ys
                      | otherwise = y : removeFstOcu x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []     _     = True
isChoice (x:xs) ys = elem x ys && isChoice xs (removeFstOcu x ys)


-- ex4

ns = [1,3,7,10,25,50]
total = 33665406
success = 4672540

testEx4_1 = length [e | xs <- choices' ns, e <- exprs xs] == total

testEx4_2 = length [r | xs <- choices' ns, e <- exprs xs, r <- eval e] == success



-- ex5

success' = 10839369

eval' :: Expr -> [Int]
eval' (Val n)     = [n]
eval' (App o l r) = [apply o x y | x <- eval' l,
                                   y <- eval' r,
                                   valid' o x y]

valid' :: Op -> Int -> Int -> Bool
valid' Add _ _ = True
valid' Sub _ _ = True
valid' Mul _ _ = True
valid' Div x y = y /= 0 && x `mod` y == 0

testEx5 = length [r | xs <- choices' ns, e <- exprs xs, r <- eval' e] == success'


-- ex6

