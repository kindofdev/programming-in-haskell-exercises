module Ex04 where

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show
 
instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf         = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)  

instance Foldable Tree where
  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b 
  foldMap _ Leaf         = mempty
  foldMap g (Node l x r) = foldMap g l `mappend` g x `mappend` foldMap g r 
  
instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse g Leaf         = pure Leaf
  traverse g (Node l x r) = pure Node <*> traverse g l <*> g x <*> traverse g r  
  
  
dec :: Int -> Maybe Int  
dec n = if n > 0 then Just (n-1) else Nothing   