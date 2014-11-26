halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
            where n = (length xs) `div` 2

mult x y z = x * y * z
a = \x -> \y -> \z -> x * y * z

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

