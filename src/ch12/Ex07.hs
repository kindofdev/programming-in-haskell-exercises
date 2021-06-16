module Ex07 where
  
import Data.Char

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show

instance Functor Expr where
--  fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var x)   = Var (g x)  
  fmap _ (Val n)   = Val n 
  fmap g (Add l r) = Add (fmap g l) (fmap g r)  


instance Applicative Expr where
--  pure :: a -> Expr a
  pure = Var
  
--  (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  _         <*> (Val n)   = Val n  
  (Val n)   <*> _         = Val n 
  (Var g)   <*> (Var x)   = Var (g x)
  (Var g)   <*> (Add l r) = Add (Var g <*> l) (Var g <*> r)  
  (Add l r) <*> e         = Add (l <*> e) (r <*> e)    


instance Monad Expr where
  return = Var  
  
--  (>>=) :: Expr a  -> (a -> Expr b) -> Expr b
  (Val n)   >>= _ = Val n 
  (Var x)   >>= g = g x
  (Add l r) >>= g = Add (l >>= g) (r >>= g)
  

-- Examples

e :: Expr Char
e = Add (Add (Var 'c') (Var 'd')) (Val 3)

test1 :: Expr Int
test1 = e >>= (Val . ord)

test2 :: Expr String
test2 = e >>= (\c -> Add (Var "foo") (Var "bar"))

