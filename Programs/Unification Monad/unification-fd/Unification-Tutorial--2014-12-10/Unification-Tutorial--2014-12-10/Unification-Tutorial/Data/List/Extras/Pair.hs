module Data.List.Extras.Pair (pairWith) where

pairWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
pairWith f = go where
  go [] []  = Just []
  go (x:xs) (y:ys) = do
    let z = f x y
    zs <- go xs ys
    return (z:zs)
  go _ _ = Nothing
