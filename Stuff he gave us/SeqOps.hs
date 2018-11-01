{- CSci 450/503: Org. of Programming Languages, Seq Ops Module
   Fall 2017, Examination #2, Problems 10(a), 11
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2007-04-26: Previous development of Seq data type and operations
2017-10-19: Expanded, modularized, separated testing
2017-10-21: Updated comments to reflect functions added to exam

Functions headSeq, seqToList, filterSeq, foldrSeq, and printSeq and
Eq instance declaration were not included in the exam question.

-}

module SeqOps  -- 10(a) "module" through "where"
  ( Seq(..), isNil, headSeq, tailSeq, listToSeq, seqToList, lenSeq,
    appSeq, revSeq, eqSeq, mapSeq, filterSeq, foldrSeq, printSeq
  )
where

data Seq a = Nil | Cons a (Seq a)
             deriving Show

-- (a)
isNil :: Seq a -> Bool
isNil Nil = True
isNil _   = False

-- not used
headSeq :: Seq a -> a
headSeq (Cons x xs) = x

-- (b)
tailSeq :: Seq a -> Seq a
tailSeq (Cons x xs) = xs

-- (g)
listToSeq :: [a] -> Seq a
listToSeq []     = Nil
listToSeq (x:xs) = Cons x (listToSeq xs)

-- not used
seqToList :: Seq a -> [a]
seqToList Nil         = []
seqToList (Cons x xs) = x : seqToList xs

-- given in problem description
lenSeq :: Seq a -> Int
lenSeq Nil         = 0
lenSeq (Cons x xs) = 1 + lenSeq xs

-- (d)
revSeq :: Seq a -> Seq a
revSeq Nil = Nil
revSeq (Cons x xs) = rev' xs $ Cons x Nil
    where rev' Nil ys         = ys
          rev' (Cons x xs) ys = rev' xs $ Cons x ys

-- (c)
appSeq :: Seq a -> Seq a -> Seq a
appSeq Nil         ys = ys
appSeq (Cons x xs) ys = Cons x (appSeq xs ys)

-- (f)
eqSeq :: Eq a => Seq a -> Seq a -> Bool
eqSeq Nil         Nil         = True
eqSeq (Cons x xs) (Cons y ys) = (x == y) && eqSeq xs ys
eqSeq _           _           = False

-- (e)
mapSeq :: (a -> b) -> Seq a -> Seq b
mapSeq f  Nil        = Nil
mapSeq f (Cons x xs) = Cons (f x) (mapSeq f xs)

-- notUsed
filterSeq :: (a -> Bool) -> Seq a -> Seq a
filterSeq p Nil = Nil
filterSeq p (Cons x xs)
    | p x       = Cons x (filterSeq p xs)
    | otherwise = filterSeq p xs

-- not used
foldrSeq :: (a -> b -> b) -> b -> Seq a -> b
foldrSeq _ z Nil         = z
foldrSeq f z (Cons x xs) = f x (foldrSeq f z xs)

-- not used (not discussed IO chapter)
printSeq :: Show a => Seq a -> IO ()
printSeq s@(Nil)     = putStrLn (show s)
printSeq (Cons x xs) =
    do  putStrLn (show x)
        printSeq xs

-- not used (not yet discussed type class chapter)
instance Eq a => Eq (Seq a) where
    (==) = eqSeq
