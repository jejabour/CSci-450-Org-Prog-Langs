{- CSci 450/503, Fall 2018
   Homework #4:  Exam DSL Project Code Skeleton

-}

module ExamDSL 
where

-- instructor used these in his solution
import Data.List ( delete, intercalate, intersect, (\\), elemIndex, findIndex)
import Data.Char
import Data.Random

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


-- Exercise Set A

-- #A1
correctChoice :: Choice -> Bool
correctChoice (Answer _ b) 
    |b = True
    |otherwise = False

-- correctChoiceNum :: Question -> Maybe Int
-- correctChoiceNum (Ask tag qtext (choice:choices)) 
--     | not (correctChoice choice)  = Nothing
--     | otherwise = findIndex (correctChoice choices)
    

-- #A2
lenQuestion :: Question -> Int
lenQuestion (Ask _ _ qs) = length qs


-- #A3
validQuestion :: Question -> Bool
validQuestion (Ask as w (q:qs))
    |null as || null w || null qs = False
    |lenQuestion (Ask as w qs) < 2 = False
    |lenQuestion (Ask as w qs) > 10 = False
    -- |trueChoices qs x /= 1 = False
    |otherwise = True
        -- where x = 0

-- trueChoices :: Choice -> Int -> Int
-- trueChoices (q:qs) x
--     |q == True = x + 1
--     |otherwise trueChoices (qs x) = 0


-- #A4
hasTag :: Question -> Tag -> Bool
hasTag (Ask tag _ _) t = t `elem` tag

-- #A5
eqBag :: Eq a => [a] -> [a] -> Bool
eqBag xs ys
    | null (xs \\ ys) = True
    | null (ys \\ xs) = True
    | otherwise = False

-- #A6
instance Eq Question where
    (Ask tag1 qtext1 choice1) == (Ask tag2 qtext2 choice2) = eqBag tag1 tag2 && eqBag choice1 choice2 && qtext1 == qtext2

-- data Exam     = Quiz Title [Question] deriving Show
-- data Question = Ask [Tag] QText [Choice] deriving Show

-- #A7
selectByTags :: [Tag] -> Exam -> Exam
selectByTags tag (Quiz title oldQuestionList) = Quiz title newQuestionList
    where newQuestionList = filter (findTags tag) oldQuestionList

findTags :: [Tag] -> Question -> Bool
findTags tag (Ask tagList qText choiceList) 
    | length (tag \\ tagList) < length tag = True
    | otherwise = False

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
-- makeKey (Quiz _ (q:qs)) = 
--     [(number, letter) | number <- [1 .. length qs], 
--     letter <- ['A', 'B' .. lenQuestion q]]


-- makeKeyTest :: Exam -> [Int]
-- makeKeyTest (Quiz _ questionList) = [number | number <- [1, 2 .. length questionList]]

makeKeyTest3 :: Exam -> [(Int,Char)]
makeKeyTest3 (Quiz _ (q:qs)) = 
    [(number, letter) | number <- [1 .. length qs]]
    where
        letterList = [1 .. lenQuestion q]
        letterList2 = [letter | letter <- map intToDigit letterList]
        letter = sample $ randomElement letterList2


-- makeKeyTest2 :: Exam -> [(Int,Char)]
-- makeKeyTest2 (Quiz _ questionList) = 
--     [(number, letter) | number <- [1 .. length questionList]]
--         where 
--             letterlist = ['A', 'B' .. 'I']
--             letter = length questionList `elemIndex` 

-- findCorrectNum :: Question -> Int -> (Int -> Int) -> Int
-- findCorrectNum (Ask _ _ c:cs) number f 
--             | val >= number = val
--             | otherwise = findCorrectNum (start + 1) number filter
--                 where  
--                 val = f start

-- findCorrectNum' :: Question -> Int
-- findCorrectNum' (Ask tag qtext (c:cs))
--     | correctChoice c = val
--     | otherwise = findCorrectNum' (Ask tag qtext cs) (succ val)
--         where val = 0
    


-- data Exam     = Quiz Title [Question] deriving Show
-- data Question = Ask [Tag] QText [Choice] deriving Show


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

q1 = Ask ["curriculum"]
            "Which one of the following is a required course?"
            [ Answer "CSci 323" False, 
            Answer "CSci 450" True, 
            Answer "CSci 523" False,
            Answer "CSci 524" False,
            Answer "CSci 525" False,
            Answer "CSci 526" False,
            Answer "CSci 527" False,
            Answer "CSci 528" False,
            Answer "CSci 529" True]
    
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

e1 = Quiz "Curriclum Test" [
            Ask ["curriculum1"]
                "Which one of the following courses is required?"
                [ Answer "CSci 323" False, 
                Answer "CSci 450" True, 
                Answer "CSci 525" False ],
            Ask ["language","course2"]
                "What one of the following is used in CSci 450?"
                [ Answer "Lua" False,
                Answer "Elm" False,
                Answer "Haskell" True ],
            Ask ["curriculum3"]
                "Which one of the following courses is required?"
                [ Answer "CSci 323" False, 
                Answer "CSci 450" True, 
                Answer "CSci 525" False ],
            Ask ["curriculum4"]
                "Which one of the following courses is required?"
                [ Answer "CSci 323" False, 
                Answer "CSci 450" True, 
                Answer "CSci 525" False ],
            Ask ["curriculum5"]
                "Which one of the following courses is required?"
                [ Answer "CSci 323" False, 
                Answer "CSci 450" True, 
                Answer "CSci 525" False ]
            ]