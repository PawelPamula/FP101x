import System.IO
import Data.Char

putStr' [] = return ()
putStr' (x: xs) = putChar x >> putStr' xs

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr "6"
            putStrLn " characters"

sgetLine :: IO String
sgetLine = do   x <- getChar
                if x == '\n' then
                    do  putChar x
                        return []
                else
                    do  putChar '-'
                        xs <- sgetLine
                        return (x: xs)

getCh :: IO Char
getCh = do  hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

guess :: String -> IO ()
guess word = 
    do  putStr ">"
        xs <- getLine
        if xs == word then
            putStrLn "Great"
        else
            do  putStrLn (diff word xs)
                guess word

diff :: String -> String -> String
diff xs ys = [if elem x ys then x else '-' | x <- xs]

getLine' :: IO String
getLine' = get []

get :: String -> IO String
get xs = 
    do  x <- getChar
        case x of
            '\n' -> return xs
            _ -> get (xs ++ [x])


interact' f
    = do    input <- getLine'
            putStr' (f input)