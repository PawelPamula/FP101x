all' p xs = foldl (&&) True (map p xs)

any' p = not . null . filter p

takeWhile' _ [] = []
takeWhile' p (x : xs)
    | p x = x : takeWhile p xs
    | otherwise = []

filter' p = foldr (\ x xs -> if p x then x : xs else xs) []

dec2int = foldl (\x y -> 10*x + y) 0

compose' = foldr (.) id

curry' f = \ (x, y) -> f x y

type Bit = Int

unfold p h t x
    | p x = []
    | otherwise = h x : unfold p h t (t x)

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

chop8 = unfold null (take 8) (drop 8)

map' f = unfold null (f . head) tail
iterate' f = unfold (const False) id f