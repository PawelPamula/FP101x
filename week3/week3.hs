-- list comprehensions

import Data.Char

sum100 = sum [x^2 | x <- [1..100]]
sum100' = foldl (+) 1 [x ^ 2 | x <- [1..100]]

replicate' n a = [a | _ <- [1..n]]

pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

factors n = [i | i <- [1..n], n `mod` i == 0]

perfect n = [x | x <- [1..n], isPerfect x]
                where isPerfect num = sum (init (factors num)) == num

pairs = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]
pairs' = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: (Eq a) => a -> [a] -> [Int]
-- positions 2 [1, 2, 3, 4, 5, 6, 7]
positions x xs = find x (zip xs [0..n])
                    where n = length xs - 1

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]



let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

xs = 1 : [x+1 | x <- xs]

riffle xs ys = concat [[x, y] | (x, y) <- xs `zip` ys]

divisors x = [d | d <- [1..n], d `divides` x]