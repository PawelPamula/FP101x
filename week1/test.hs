
add :: Int -> (Int -> Int)
add x y = x + y

a :: (Int, Int) -> Int
a (x, y) = x + y

second xs = head (tail xs)
swap (x, y) = (y, x)

double x = x * 2

palindrome xs = reverse xs == xs

twice f x = f (f x)

f xs = take 3 (reverse xs)