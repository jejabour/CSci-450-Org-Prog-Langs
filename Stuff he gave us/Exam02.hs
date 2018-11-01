{- CSci 450/503 Fall 2017
   Examination #2
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-10-21: First version

To see types, give ghci command :set +t

-}

module Exam02
where

-- Problem 5

xsss = [["hugs","and"],["kisses"],["from","Hersheys"]]
p5a  = length xsss
p5b  = head xsss
p5c  = foldr (++) [] xsss
p5d  = [ xs | xss <- xsss, xs <- xss, length xs > 0 ]
p5e  = filter (==1) $ map length xsss
p5f  = (head . tail . head . tail . tail) xsss

-- Problem 6

p6a  = [1,3..9]
p6b  = [9,7..1]
p6c  = take 5 [ i | i <- [1..], odd i ]
p6d  = [ (x,y) | x <- [1..2], y <- [0,2] ]
p6e  = [ [1..n] | n <- [1..3] ]
p6f  = [ [ x | x <- [1..y] ] | y <- [1..3] ]

-- Problems 7-9
  
data Tree a = Leaf | Node a (Tree a) (Tree a) 

inorder Leaf                = []
inorder (Node v left right) = inorder left ++ [v] ++ inorder right

-- Problem 10-11 -- See modules SeqOps, SeqTest
