import Data.List

mul :: (Num a) => a -> a -> a -> a
mul x y z = x * y * z

comp :: (Num a, Ord a) => a -> Ordering
comp x = compare 100 x

applyTwice :: (a -> a) -> (a -> a)
applyTwice f = f . f

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

