module Ex02 where

instance Monoid b => Monoid (a -> b) where
  -- mempty :: (a -> b)
  mempty = const mempty
  
  -- mappend :: (a -> b) -> (a -> b) -> (a -> b) 
  mappend f g = \x -> f x `mappend` g x