module Ex03 where

instance Foldable Maybe where      
  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b        
  foldMap _ Nothing  = mempty 
  foldMap f (Just x) = f x
  
  -- fold :: Monoid a => Maybe a -> a
  fold Nothing  = mempty
  fold (Just x) = x
    
  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b    
  foldr _ y Nothing  = y
  foldr f y (Just x) = f x y 
  
  -- foldl :: (b -> a -> b) -> b -> Maybe a -> b
  foldr _ y Nothing  = y
  foldr f y (Just x) = f y x 
  

instance Traversable Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Nothing  = pure Nothing     
  traverse g (Just x) = pure Just <*> g x    