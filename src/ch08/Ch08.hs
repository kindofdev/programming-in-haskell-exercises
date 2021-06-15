module Ch8 where

-- ex1

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult m Zero        = Zero
mult m (Succ n)    = add m (mult m n)

n3 = Succ (Succ (Succ Zero))
n2 = Succ (Succ Zero)
n1 = Succ Zero



-- ex2

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = case compare x y of
                           LT -> occurs x l
                           EQ -> True
                           GT -> occurs x r


-- ex3

data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a) deriving Show

t1 :: Tree2 Int
t1 = Node2 (Node2 (Leaf2 1) (Leaf2 4))
          (Node2 (Leaf2 6) (Leaf2 9))

t2 :: Tree2 Int
t2 = Node2 (Leaf2 4)
           (Node2 (Leaf2 6) (Node2 (Leaf2 9) (Leaf2 8)))

leaves :: Tree2 a -> Int
leaves (Leaf2 _)   = 1
leaves (Node2 l r) = leaves l + leaves r

balanced :: Tree2 a -> Bool
balanced (Leaf2 _)   = True
balanced (Node2 l r) = abs (leaves l - leaves r) <= 1
                       && balanced l
                       && balanced r


-- ex4

halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree2 a
balance [x] = Leaf2 x
balance xs  = Node2 (balance ys) (balance zs)
              where (ys, zs) = halve xs


-- ex5

data Expr = Val Int | Add Expr Expr deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)     = f x
folde f g (Add e1 e2) = g (folde f g e1) (folde f g e2)

expr1 = Add (Val 2) (Add (Val 4) (Add (Val 6) (Val 8)))

-- ex6

evalEx :: Expr -> Int
evalEx = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)


-- ex7


--instance Eq a => Eq (Maybe a) where
--  (==) Nothing  Nothing  = True
--  (==) (Just x) (Just y) = x == y
--  (==) _        _        = False
--
--instance Eq a => Eq [a] where
--  (==) []     []     = True
--  (==) (x:xs) (y:ys) = x == y && xs == ys
--  (==) _      _      = False


-- ex8Data.Boolean

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | XNOR Prop Prop 

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (XNOR p q)  = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (XNOR p q)  = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

subts :: Prop -> [Subst]
subts p = map (zip vs) (bools $ length vs)
          where vs = rmdups $ vars p

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- subts p]


-- testing ex8

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

pDis :: Prop
pDis = Or (Var 'A') (Not (Var 'A'))
--pDis = Or (Var 'A') (Var 'B')

pEq :: Prop
pEq = XNOR (Var 'A') (Var 'A')

-- ex9 TODO