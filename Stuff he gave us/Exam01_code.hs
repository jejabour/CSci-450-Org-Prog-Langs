{- CSci 450/503 Fall 2017
   Examination #1
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-09-21: First version

To see types, give ghci command :set +t

-}

module Exam01
where

rad :: Eq a => [a] -> [a]
rad (x : xs@(y:_))
    | x == y = rad xs
    | x /= y = x : rad xs
rad xs = xs       

cav :: [Double] -> Double
cav (x:xs) = cavaux xs (x,1)
    where cavaux [] (s,c)     = s / c
          cavaux (x:xs) (s,c) = cavaux xs (s+x,c+1)

{- Define the following set of text-justification functions.
   You may want to use Prelude functions like take, drop,
   ++, and `length` as well as pattern matching, cons, etc.
-}

-- spaces n returns a string of length n containing only space
-- characters (i.e., the character `’ ’`).

spaces :: Int -> String
spaces n
   | n <= 0    = ""
   | otherwise = ' ' : spaces (n-1)
   
-- left n xs returns a string of length n in which the string
-- xs begins at the head (i.e., left end).
-- Examples: left 3 "ab" yields "ab "
-- left 3 "abcd" yields "abc"

left :: Int -> String -> String
left n _ | n <= 0  = []
left n []          = spaces n
left n (x:xs)      = x: left (n-1) xs
  
