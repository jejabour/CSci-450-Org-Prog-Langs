-- Joseph Jabour, CSci 450 Organization of Programming Languages with Dr. Cunningham
-- Homework 01, Assignment 1
-- Brief assignment instruction summary: Create the Haskell funcations prodSqSmall, xor, implies, ccArea, addTax, subTax, validDay, and roman
-- Citations:
-- The first five or so chapters from Learn You a Haskell for Great Good by Miran Lipovaˇca were used to learn the Haskell language
-- Anything else used was probably found in Stack Overflow and used or modified heavily to fit my needs for the assignment
-- Also I apologize for validDay, I know it's bad

main = do
    putStrLn ""
    putStrLn "Welcome to Joseph Jabour's HW01! There are several functions to choose from."
    putStrLn "I'll list each function followed by the format needed to properly use each."
    putStrLn "An underscore '_' implies to input a numeric value."
    putStrLn "If a function calls for a Boolean, 'True/False' will indicate as such."
    putStrLn ""
    putStrLn "'prodSqSmall _ _ _'"
    putStrLn "This function will find the smallest of three given doubles, square each, then add their sums."
    putStrLn ""
    putStrLn "'xor 'True/False' 'True/False''"
    putStrLn "This function will take two Booleans and use the Exclusive Or logic on it."
    putStrLn ""
    putStrLn "'implies 'True/False' 'True/False''"
    putStrLn "This function uses the Implies logic, meaning that it will only return True as long as the first and second are not True and False, respectively."
    putStrLn ""
    putStrLn "'ccArea _ _'"
    putStrLn "This function will take two circumferences and find the area of the space in between them."
    putStrLn ""
    putStrLn "'addTax _ _'"
    putStrLn "This function will take two doubles, the first being a price value, the second being desired tax. The function will then apply tax to the given value."
    putStrLn ""
    putStrLn "'subTax _ _'"
    putStrLn "Similar to the addTax function, this function takes two doubles, a value with tax added, and the tax that was added. The function will then remove the tax from the first value."
    putStrLn ""
    putStrLn "'validDay (_, _, _)'"
    putStrLn "This function will take three integer values and determine if they make up a valid Gregorian calendar date, using the following format: (Month, Day, Year)."
    putStrLn ""
    putStrLn "'roman _'"
    putStrLn "This function will take a single integer value and convert it to Roman Numerals."
    putStrLn ""

-- module HW01  -- in file nameed HW01.hs
--    ( prodSqSmall, xor, implies, ccArea, addTax, subTax,
--      validDay, roman )
-- where

-- -- ELIFP Ch. 5, Ex. 2
-- prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall x y z = 
    if x > y && x > z
        then (z^2) + (y^2)
    else if y > x && y > z
        then (x^2) + (z^2)
    else (x^2) + (y^2)


-- -- ELIFP Ch. 5, Ex. 3
-- xor :: Bool -> Bool -> Bool
xor :: Bool -> Bool -> Bool
xor x y 
    | x == True && y == False = True
    | x == False && y == True = True
    | otherwise = False


-- -- ELIFP Ch. 5, Ex. 4
-- implies :: Bool -> Bool -> Bool
implies :: Bool -> Bool -> Bool
implies p q
    | p == True && q == False = False
    | otherwise = True


-- -- ELIFP Ch. 5, Ex. 7
-- ccArea :: Double -> Double -> Double
ccArea :: Double -> Double -> Double
ccArea x y = 
    if x > y
        then (x / 2 * pi * 2) - (y / 2 * pi * 2)
    else (y / 2 * pi * 2) - (x / 2 * pi * 2)

    
-- -- ELIFP Ch. 5, Ex. 9
-- -- Consider the equation:  addTax c p = ct 
-- addTax :: Double -> Double -> Double
addTax :: Double -> Double -> Double
addTax x y = x + (x * (y / 100))

-- subTax :: Double -> Double -> Double
subTax :: Double -> Double -> Double
subTax x y = x / (1 + ( y / 100))


-- -- ELIFP Ch. 5, Ex.11
-- -- Using proleptic Gregorian calendar, which underlies ISO 8601 standard
-- validDay :: (Int,Int,Int) -> Bool
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


-- -- ELIFP Ch. 5, Ex. 12
-- -- Roman numerals in range [0-3999], where 0 represented as empty string
-- roman :: Int -> String
testMap = [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]

roman :: Integer -> String
roman 0 = "N"
roman x = calcroman x

calcroman :: Integer -> String
calcroman x 
  | x == 0 = ""
  | x > 0 = y ++ calcroman (x - z)
      where (z, y) = head $ filter ((<= x) . fst) testMap
