{- Carrie's Candy Bowl, Instructor List Representation Solution
   H. Conrad Cunningham
   
1234567890123456789012345678901234567890123456789012345678901234567890

2018-10-21: Expanded from similar previous work
2018-10-21: Revised order to match CSci 450/503 Assignment #3

Notes:

-   In some cases, you may need to restrict the polymorphism to
    implement a function. Be careful not to restrict functions
    unnecessarily.

-   You may find Prelude functions such as concatMap, elem, filter,
    length, map, null, replicate, and span useful.

-   You may also find functions in the Data.List library
    useful -- e.g. sort, group, and (\\).

-}

module CandyBowl_list
  ( CandyBowl(..), newBowl, putIn, takeOut, isEmpty, size, howMany,
    has, eqBowl, inventory, restock, combine, difference, rename,
    inventory', restock', difference' -- added to instructor solution
  )
where

-- Used in instructor solution
import Data.List ( sort, group, (\\) )

-- Candy bowl data representation
data CandyBowl a = Bowl [a] deriving Show


-- Exercise #1
newBowl :: CandyBowl a
newBowl = Bowl []


-- Exercise #2
isEmpty :: CandyBowl a -> Bool
isEmpty (Bowl []) = True
isEmpty _         = False


-- Exercise #3
putIn :: CandyBowl a -> a -> CandyBowl a
putIn (Bowl cs) c = Bowl (c:cs)


-- Exercise #4
has :: Eq a => CandyBowl a -> a -> Bool
has (Bowl cs) c = elem c cs


-- Exercise #5
size :: CandyBowl a -> Int
size (Bowl cs) = length cs


-- Exercise #6
howMany :: Eq a => CandyBowl a -> a -> Int
howMany (Bowl cs) c = length (filter (==c) cs)


-- Exercise #7
takeOut :: (Eq a,Show a) => CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut b@(Bowl cs) c
    | has b c   = Just (Bowl (xs ++ tail ys) )                    
    | otherwise = Nothing
    where (xs,ys) = span (/= c) cs


-- Exercise #8
eqBowl :: Ord a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl cs1) (Bowl cs2) = (sort cs1) == (sort cs2)


-- Exercise #9                            
inventory :: Ord a => CandyBowl a -> [(a,Int)]
inventory (Bowl cs) = map tuplize $ group $ sort cs
    where tuplize xs = (head xs, length xs)

-- version without library functions map and group 
inventory' :: Ord a => CandyBowl a -> [(a,Int)]
inventory' (Bowl cs) = tuplize (sort cs)
    where tuplize []      = []
          tuplize (c:cs)  = inv cs c 1

          inv [] b n      = [(b,n)]
          inv (x:xs) b n
              | x == b    = inv xs b (n+1)
              | otherwise = (b,n) : inv xs x 1


-- Exercise #10
restock :: [(a,Int)] -> CandyBowl a
restock inv = Bowl (concatMap detuplize inv)
              where detuplize (c,n) = replicate n c

-- version without library function concatMap
restock' :: [(a,Int)] -> CandyBowl a
restock' cs = Bowl (stock cs)
    where stock []         = []
          stock ((c,n):bs) = replicate n c ++ stock bs


-- Exercise #11
combine :: CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl cs1) (Bowl cs2) = Bowl (cs1 ++ cs2)


-- Exercise #12
difference :: Eq a => CandyBowl a -> CandyBowl a -> CandyBowl a
difference (Bowl cs1) (Bowl cs2) = Bowl (cs1 \\ cs2)

-- version without library function \\
difference' :: Eq a => CandyBowl a -> CandyBowl a -> CandyBowl a
difference' (Bowl cs1) (Bowl cs2) = Bowl (diff cs1 cs2)
    where diff [] _       = []   -- implements \\
          diff xs []      = xs
          diff xs (y:ys)  = diff (remove y xs) ys

          remove v []     = []
          remove v (w:ws)
              | v == w    = ws
              | otherwise = w : remove v ws


-- Exercise #13
rename :: CandyBowl a -> (a -> b) -> CandyBowl b
rename (Bowl cs) f = Bowl (map f cs)
