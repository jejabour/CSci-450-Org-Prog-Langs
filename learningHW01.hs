import Data.Time.Calendar

main = putStrLn "Hello World!"

doubleMe x = x + x
doubleUs x y = x * 2 + y * 2


findMin1 x y z = minimum [x, y, z]


lucky :: Int -> String
lucky 7 = "LUCKY SEVEN!"
lucky x = "Nope"

factorial :: Int -> Int
factorial 0=1
factorial n = n * factorial (n-1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
charName x = "None"

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- first :: (a, b, c) -> a
-- first (x, _, _) = x
-- second :: (a, b, c) -> b
-- second (_, y, _) = y
-- third :: (a, b, c) -> c
-- third (_, _, z) = z

headlearn' :: [a] -> a
headlearn' [] = error "Can't call head on an empty list, dummy!"
headlearn' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

firstLetter :: String -> String
firstLetter "" = "Empty string, whoops!"
firstLetter all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: Double -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, eat more!"
    | bmi <= 25.0 = "Looking good!"
    | bmi <= 30.0 = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."

--  or

bmiTell2 :: Double -> Double -> String
bmiTell2 weight height
    | bmi <= skinny = "You're underweight, eat more!"
    | bmi <= normal = "Looking good!"
    | bmi <= overweight = "You're overweight. Let's work out together!"
    | otherwise = "You're obese. Go see a doctor."
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          overweight = 30.0

-- or

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b = b
    | otherwise = a

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a == b = EQ
    | a <= b = LT
    | otherwise = GT

-- ghci> 3 `myCompare` 2
-- GT


badGreeting :: String
badGreeting = "Oh! Pfft. It's you."

niceGreeting :: String
niceGreeting = "Hello! So very nice to see you,"

greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet "Fernando" = niceGreeting ++ " Fernando!"
greet name = badGreeting ++ " " ++ name



initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."

    where (f:_) = firstname
          (l:_) = lastname


cylinder :: Double -> Double -> Double
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

headls' :: [a] -> a
headls' [] = error "No head for empty lists!"
headls' (x:_) = x

-- Same as

head' :: [a] -> a
head' xs = case xs of [] -> error "No head for empty lists!"
                      (x:_) -> x

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- This will be min if you change that max at the bottom to min
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Also I'm on page 58

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallerOrEqual ++ [x] ++ quicksort larger
    

-- prodSqSmall :: (Ord a) => [a] -> [a]
-- prodSqSmall [] = []
-- prodSqSmall (x:xs) = 
--     let smallerOrEqual = [a | a <- xs, a <= x]
--         larger = [a | a <- xs, a > x]
--     in prodSqSmall smallerOrEqual ++ [x] ++ prodSqSmall larger
-- prodSqSmall (x:xs) = init prodSqSmall[x:xs]





-- 2
prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall x y z = 
    if x > y && x > z
        then (z^2) + (y^2)
    else if y > x && y > z
        then (x^2) + (z^2)
    else (x^2) + (y^2)

-- 3
xor :: Bool -> Bool -> Bool
xor x y 
    | x == True && y == False = True
    | x == False && y == True = True
    | otherwise = False

-- 4
implies :: Bool -> Bool -> Bool
implies p q
    | p == True && q == False = False
    | otherwise = True

-- 7
ccArea :: Double -> Double -> Double
ccArea x y = 
    if x > y
        then (x / 2 * pi * 2) - (y / 2 * pi * 2)
    else (y / 2 * pi * 2) - (x / 2 * pi * 2)

-- 9
addTax :: Double -> Double -> Double
addTax x y = x + (x * (y / 100))

subTax :: Double -> Double -> Double
subTax x y = x / (1 + ( y / 100))

-- 11
validDay :: (Int,Int,Int) -> Bool
validDay (month, day, year)
    | year <= 0 = False
    | month <= 0 || month > 12 = False
    | day <= 0 || day > 31 = False
    | month == 1 || month == 3 || month == 5 || month == 7 || month == 8 || month == 10 || month == 12 && day >= 1 && day <= 31 && year > 0 = True
    | month == 4 || month == 6 || month == 9 || month == 11 && day >= 1 && day <= 30 && year > 0 = True
    | month == 2 && day >= 1 && day <= 28 && year > 0 = True
    | month == 2 && day >= 1 && day <= 29 && year `rem` 400 == 0 = True
    | month == 2 && day >= 1 && day <= 29 && year `rem` 100 == 0 = False
    | month == 2 && day >= 1 && day <= 29 && year `rem` 4 == 0 = True
    | otherwise = False

-- 12
testMap = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

roman :: Integer -> String
roman 0 = "N"
roman x = calcroman x

calcroman :: Integer -> String
calcroman x 
  | x == 0 = ""
  | x > 0 = y ++ calcroman (x - z)
      where (z, y) = head $ filter ((<= x) . fst) testMap


-- bmiTell2 :: Double -> Double -> String
-- bmiTell2 weight height
--     | bmi <= skinny = "You're underweight, eat more!"
--     | bmi <= normal = "Looking good!"
--     | bmi <= overweight = "You're overweight. Let's work out together!"
--     | otherwise = "You're obese. Go see a doctor."
--     where bmi = weight / height ^ 2
--             skinny = 18.5
--             normal = 25.0
--             overweight = 30.0

-- 
-- 


-- module HW01  -- in file nameed HW01.hs
--    ( prodSqSmall, xor, implies, ccArea, addTax, subTax,
--      validDay, roman )
-- where

-- -- ELIFP Ch. 5, Ex. 2
-- prodSqSmall :: Double -> Double -> Double -> Double

-- -- ELIFP Ch. 5, Ex. 3
-- xor :: Bool -> Bool -> Bool

-- -- ELIFP Ch. 5, Ex. 4
-- implies :: Bool -> Bool -> Bool

-- -- ELIFP Ch. 5, Ex. 7
-- ccArea :: Double -> Double -> Double
    
-- -- ELIFP Ch. 5, Ex. 9
-- -- Consider the equation:  addTax c p = ct 
-- addTax :: Double -> Double -> Double

-- subTax :: Double -> Double -> Double

-- -- ELIFP Ch. 5, Ex.11
-- -- Using proleptic Gregorian calendar, which underlies ISO 8601 standard
-- validDay :: (Int,Int,Int) -> Bool

-- -- ELIFP Ch. 5, Ex. 12
-- -- Roman numerals in range [0-3999], where 0 represented as empty string
-- roman :: Int -> String