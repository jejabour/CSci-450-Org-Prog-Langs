{- Carrie's Candy Bowl, Instructor Solution
   H. Conrad Cunningham
   
1234567890123456789012345678901234567890123456789012345678901234567890

2018-10-21: Adapted from list version
2018-10-21: Revised order to match CSci 450/503 Assignment #3

-}

module CandyBowl_map
  ( CandyBowl(..), newBowl, putIn, takeOut, isEmpty, size, howMany,
    has, eqBowl, inventory, restock, combine, difference, rename
  )
where

-- import Data.List ( sort, group, (\\) )
import qualified Data.Map.Strict as M

-- Candy bowl data representation
-- data CandyBowl a = Bowl [a] deriving Show
data CandyBowl a = Bowl (M.Map a Int) deriving Show


-- Exercise #1
newBowl :: Ord a => CandyBowl a
newBowl = Bowl M.empty


-- Exercise #2
isEmpty :: Ord a => CandyBowl a -> Bool
isEmpty (Bowl b) = M.null b


-- Exercise #3
putIn :: Ord a => CandyBowl a -> a -> CandyBowl a
putIn (Bowl b) c
    | M.notMember c b = Bowl (M.insert c 1 b)
    | otherwise       = Bowl (M.adjust (+1) c b)


-- Exercise #4
has :: Ord a => CandyBowl a -> a -> Bool
has (Bowl b) c = M.member c b


-- Exercise #5
size :: Ord a => CandyBowl a -> Int
size (Bowl b) = sum (M.elems b)


-- Exercise #6
howMany :: Ord a => CandyBowl a -> a -> Int
howMany (Bowl b) c =
    case M.lookup c b of
        Nothing -> 0
        Just n  -> n

-- Exercise #3
takeOut :: (Ord a,Show a) => CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut (Bowl b) c =
    case M.lookup c b of
        Nothing -> Nothing
        Just n  -> if n > 0 then Just (Bowl (M.insert c (n-1) b))
                            else Just (Bowl (M.delete c b))


-- Exercise #8
eqBowl :: Ord a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl b1) (Bowl b2) = b1 == b2


-- Exercise #9                            
inventory :: Ord a => CandyBowl a -> [(a,Int)]
inventory (Bowl b) = M.toAscList b


-- Exercise #10
restock :: Ord a => [(a,Int)] -> CandyBowl a
restock inv = Bowl (M.fromList inv)

  
-- Exercise #11`
combine :: Ord a => CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl b1) (Bowl b2) = Bowl (M.unionWith (+) b1 b2)


-- Exercise #12
difference :: Ord a => CandyBowl a -> CandyBowl a -> CandyBowl a
difference (Bowl b1) (Bowl b2) =
    Bowl (M.filter (>0) (M.differenceWith (\l r -> Just (l-r)) b1 b2))


-- Exercise #13
rename :: (Ord a, Ord b) => CandyBowl a -> (a -> b) -> CandyBowl b
rename (Bowl b) f = Bowl (M.mapKeys f b)

