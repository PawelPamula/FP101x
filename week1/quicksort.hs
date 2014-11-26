q [] = []
q (x : xs) = q larger ++ [x] ++ q smaller
    where smaller = [a | a <- xs, a <= x]
          larger = [b | b <- xs, b > x]
