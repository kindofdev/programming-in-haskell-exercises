module Ex08 where
  
type State = Int

newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st


instance Functor ST where
--  fmap :: (a -> b) -> ST a -> ST b
  fmap g st = do x <- st
                 return (g x)
    
instance Applicative ST where
--  pure :: a -> ST a
  pure x = S (\s -> (x,s)) 
  
--  (<*>) ST (a -> b) -> ST a -> ST b    
  stf <*> stx = do f <- stf
                   x <- stx
                   return (f x)
    
instance Monad ST where
  return = pure
  
--  (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f = S (\s ->
                 let (x, s') = app st s in app (f x) s')  
