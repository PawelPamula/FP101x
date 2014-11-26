-- Conditional expression

abs_ :: Int -> Int
abs_ n = if n >= 0 then n else -n

signum_ :: Int -> Int
signum_ n =  if n < 0 then -1 else
                if n == 0 then 0 else 1

a n | n >= 0    = n
    | otherwise = -n

-- Lambda expressions

addition = \x -> (\y -> x + y)

odds n = map (\x -> x*2 + 1) [0..n-1]

-- Sections

fun = (+) 10

inc = (1+)

-- Functions

halve' xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
    where n = length xs

halve'' xs = (take n xs, drop n xs)
    where n = length xs `div` 2

halve xs = splitAt (div (length xs) 2) xs

-- Safetail

safetail' xs = if null xs then [] else tail xs

safetail 
    = \ xs ->
        case xs of
            [] -> []
            (_ : xs) -> xs


-- ----

mult = (\x -> (\y -> (\z -> x * y * z)))

remove n xs = take (n + 1) xs ++ drop n xs

e1 = [[1, 2], [1, 2]]

e9 :: Int -> Int -> Int
e9 x y = x * x - x