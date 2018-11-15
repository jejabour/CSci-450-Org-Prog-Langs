-- Joseph Jabour, CSci 450 Organization of Programming Languages with Dr. Cunningham
-- Assignment 3
-- Citations
-- I used the skeleton that was given
-- The tutor Khoa helped me with a LOT of this. Most of it, he walked me through
-- how I should be doing something without ever directly saying how to do
-- it. I used stack overflow loosely, for some operators or whatever.
-- My hasList and unique functions are actually straight from Stack Overflow
-- though. Here's the URL
-- https://stackoverflow.com/questions/3098391/unique-elements-in-a-haskell-list/3098417
-- Since I'm in 450 and I had to do 1-9 and one of the last 4, I chose to 
-- do number 11, combine


module CandyBowl
  ( CandyBowl(..), newBowl, putIn, takeOut, isEmpty, size, howMany,
    has, eqBowl, inventory, combine
  )
where

-- Used in instructor solution
import Data.List ( sort, group, (\\) )

-- Candy bowl data representation
data CandyBowl a = Bowl [a] deriving Show


-- Exercise #1
-- Create a new empty CandyBowl
newBowl :: CandyBowl a
newBowl = Bowl []

-- Exercise #2
-- Check if a given CandyBowl is empty. If so, return true
isEmpty :: CandyBowl a -> Bool
isEmpty (Bowl []) = True
isEmpty (Bowl (x:xs)) = False

-- Exercise #3
-- Take a given CandyBowl and put in a given piece of candy
-- Must do (candy : xs) to put it in the first element without replacing it
putIn :: CandyBowl a -> a -> CandyBowl a
putIn (Bowl xs) candy = Bowl (candy : xs)

-- Exercise #4
-- Check the given CandyBowl and see if a given Candy is in it
-- If so, return true
has :: Eq a => CandyBowl a -> a -> Bool
has (Bowl xs) x = x `elem` xs

-- Exercise #5
-- Check the size of the given CandyBowl
size :: CandyBowl a -> Int
size (Bowl xs) = length xs

-- Exercise #6
-- Check given CB to find how many of a given element are in it
-- This basically uses a count feature. If c == x, increment
-- If not, continue
howMany :: Eq a => CandyBowl a -> a -> Int
howMany (Bowl (x:xs)) c 
    | c == x = 1 + howMany (Bowl xs) c
    | otherwise = howMany (Bowl xs) c
howMany (Bowl []) c = 0

-- Exercise #7
-- IMPORTANT: MAYBE
-- To me, Maybe is like boolean that returns Nothing or Just. 
-- Nothing if nothing, Just if something
-- I guess. 
-- This will check a bowl to see if an element is in it
-- If not, return Nothing
-- If so, return Just(the bowl with the first instance of the
-- element taken out)
takeOut :: (Eq a,Show a) => CandyBowl a -> a -> Maybe (CandyBowl a)
takeOut (Bowl []) t = Nothing
takeOut (Bowl xs) t 
    | has (Bowl xs) t == False = Nothing
    | otherwise = Just( Bowl (xs \\ [t]))

-- Exercise #8
-- Takes 2 CB's and compares them. If they're the same, return True
-- Does this by taking every element in one bowl out of the other
-- And the same vice versa, and if that makes an empty list, return true
eqBowl :: Ord a => CandyBowl a -> CandyBowl a -> Bool
eqBowl (Bowl xs) (Bowl ys)
    | null (xs \\ ys) = True
    | null (ys \\ xs) = True
    | otherwise = False

-- Exercise #9     
-- Man oh man
-- Return a list of pairs containing the element name and how many times
-- it appears in the bowl.
-- My thought process was to create a normal list containing only the
-- names of the unique elements of the bowl, then use that list to 
-- call the howMany function, and throw all of that together. Apparently,
-- the best way to recursively call a function with a list is by List
-- Comprehension, so that's what I did

-- This goes with the unique function. They both create a list of unique
-- elements from one that might have duplicates
hasList :: (Eq a) => [a] -> a -> Bool
hasList [] _ = False
hasList (x:xs) a
    | x == a    = True
    | otherwise = hasList xs a

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x:xs)
    | hasList xs x  = unique xs
    | otherwise = x : unique xs

-- This part takes the list from a bowl to call the unique function
findUnique :: Ord a => CandyBowl a -> [a]
findUnique (Bowl []) = []
findUnique (Bowl xs) = unique (sort xs)

-- The List Comprehension puts each unique element name into x,
-- and calls the howMany function on the bowl for each x
inventory :: Ord a => CandyBowl a -> [(a,Int)]
inventory (Bowl []) = []
inventory (Bowl xs) = [(x, howMany bwl x) | x <- uniq]
        where
            -- assigning the Bowl to a variable for use
            bwl = Bowl xs

            -- Creating a unique list of elements in the bowl 
            uniq = findUnique bwl

-- Exercise #11
-- Combine two CB's into one big CB
-- I'm sure there's a way to do it in one line, but it wasn't working,
-- so I made a new variable combo, then made another variable called zs
-- and set that equal to the combined bowls, then turned combo into a
-- Bowl of zs
combine :: CandyBowl a -> CandyBowl a -> CandyBowl a
combine (Bowl xs) (Bowl ys) = combo 
    where 
        zs = xs ++ ys
        combo = Bowl zs