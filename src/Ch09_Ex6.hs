import Data.List

main :: IO ()
main = print (solutionsSorted [1,2,3,4] 4)
--main = print (nearSolutions [1,3,7,10,25,50] 765)
--main = print (nearSolutions [1,3,7,10,25,50] 831)
--main = print (solutions [1,3,7,10,25,50] 831)
--main = print (solutions [1,3,7,10,25,50] 765)

data Op = Add | Sub | Mul | Div | Exp deriving (Eq, Ord) -- modified

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^" -- modified

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x <= y && x /= 1 && y /= 1
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = y > 1  -- modified

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y -- modified

data Expr = Val Int | App Op Expr Expr  deriving (Eq, Ord)

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where brak (Val n) = show n
                           brak e       = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) =  values l ++ values r

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

choices :: [a] -> [[a]]
choices = concat . map perms . subs

split :: [a] -> [([a], [a])]
split []     = []
split [_]    = []
split (x:xs) = ([x], xs) : [(x:ls, rs)| (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp] -- modified


type Result = (Expr, Int)

combine :: Result -> Result -> [Result]
combine (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls,rs) <- split ns,
                     lx      <- results ls,
                     ry      <- results rs,
                     res     <- combine lx ry]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]


nearSolutions :: [Int] -> Int -> [Expr]
nearSolutions ns n = map snd (takeWhile (\(m,_) -> m == firstRes) allSol)
                     where allSol = sort [(abs (m - n), e) | ns' <- choices ns, (e,m) <- results ns']
                           firstRes = (fst . head) allSol


-- Suitable order of simplicity --> (less values => simpler solution)

solutionsSorted :: [Int] -> Int -> [Expr]
solutionsSorted ns n = sortBy simplicity [e | ns' <- choices ns, (e,m) <- results ns', m == n]
                       where simplicity e1 e2 = compare (length $ values e1) (length $ values e2)
