import Control.Monad hiding (return)
import Prelude hiding (return)

import Data.Char
--import Prelude hiding ((>==))

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v, inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x:xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

p :: Parser (char, char)
p = do  x <- item
        item
        y <- item
        return (x, y)