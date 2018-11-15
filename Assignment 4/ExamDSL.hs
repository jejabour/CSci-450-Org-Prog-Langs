{- CSci 450/503, Fall 2018
   Homework #4:  Exam DSL Project Code Skeleton

-}

module ExamDSL 
where

-- instructor used these in his solution
import Data.List ( delete, intercalate, intersect, (\\))

import SimpleHTML (
        HTML, ListType(..), LangType(..),
        to_html, to_body, to_heading, to_list, to_li
    )

type Tag      = String
type QText    = String
data Question = Ask [Tag] QText [Choice] deriving Show

type AText    = String
data Choice   = Answer AText Bool deriving (Eq, Show)

type Title    = String
data Exam     = Quiz Title [Question] deriving Show

-- data Question' = QText [Choice] deriving (Eq, Show)

-- Exercise Set A

-- #A1
correctChoice :: Choice -> Bool
correctChoice (Answer _ b) 
    |b = True
    |otherwise = False


-- #A2
lenQuestion :: Question -> Int
lenQuestion (Ask _ _ qs) = length qs


-- #A3
validQuestion :: Question -> Bool
validQuestion (Ask as w qs)
    |null as || null w || null qs = False
    |lenQuestion (Ask as w qs) < 2 = False
    |otherwise = True

-- #A4
hasTag :: Question -> Tag -> Bool
hasTag (Ask ws _ _) t = t `elem` ws

-- #A5
eqBag :: Eq a => [a] -> [a] -> Bool
eqBag xs ys
    | null (xs \\ ys) = True
    | null (ys \\ xs) = True
    | otherwise = False


-- #A6
instance Eq Question where
    (Ask a w s) == (Ask a2 w2 s2) = (eqBag a a2) && (eqBag s s2) && (w == w2)


-- #A7
-- selectByTags :: [Tag] -> Exam -> Exam
-- selectByTags a (Quiz s q) = Quiz newS newQ
--     where newS newQ = filter (findTags a) q

-- findTags :: [a] -> Question -> Bool
-- findTags Tags (Ask tag qtext choice) = 


-- #A8
validExam :: Exam -> Bool
validExam (Quiz "" _) = False
validExam (Quiz _ []) = True
validExam (Quiz title (q:qs)) 
    | null title = False
    | not (validQuestion q) = False
    | otherwise = validExam (Quiz title qs)




-- #A9
-- makeKey :: Exam -> [(Int,Char)]



-- Exercise Set B


-- might be useful
newline :: String
newline = "\n"

-- might be useful
block :: HTML -> HTML
block str = newline ++ str ++ newline

choice2html :: Choice -> HTML
choice2html (Answer text _) = to_li text

-- #B1
-- question2html :: Question -> HTML

-- #B2
-- exam2html :: Exam -> HTML

-- Test data given in description
q0 = Ask ["curriculum"]
            "Which one of the following is a required course?"
            [ Answer "CSci 323" False, 
            Answer "CSci 450" True, 
            Answer "CSci 525" False ]
    
e0 = Quiz "Curriclum Test" [
            Ask ["curriculum"]
                "Which one of the following courses is required?"
                [ Answer "CSci 323" False, 
                Answer "CSci 450" True, 
                Answer "CSci 525" False ],
            Ask ["language","course"]
                "What one of the following is used in CSci 450?"
                [ Answer "Lua" False,
                Answer "Elm" False,
                Answer "Haskell" True ]
            ]