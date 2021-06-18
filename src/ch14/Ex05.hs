module Ex05 where

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> [x | p x])
