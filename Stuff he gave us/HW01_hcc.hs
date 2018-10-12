{- CSci 450/503, Org. of Programming Languages, Fall 2018
   Assignment #1 Solution, H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2018-09-08: Version 1
2018-09-18: Improved comments; added type aliases
2018-09-19: Added comments on ccArea, addTax about negatives
            Added alternatives xor2, implies2, ccArea2, ccArea3

For purposes of illustration of useful techniques, I used type aliases
and extended the range of validDay to negative years. For the
assignment to be graded by unit testing, you probably should not use
type aliases that are exported.

-}

module HW01_hcc  -- in file named HW01_hcc.hs
   ( prodSqSmall, xor, implies, ccArea, addTax, subTax,
     validDay, roman, -- added type aliases below to exports
     Diameter, Area, Cost, Percent, Month, Day, Year, Roman
   )
where


{- Miscellaneous hints and comments sent to students.

1.  I created a template module file that you may use. (It is linked
    to the Assignment #1 description.)

2.  I made a few minor changes to the Assignment #1 statement on the
    website.

    - Corrected a typo in exercise 9.  One occurence had "subtax"
      instead of the correct function name "subTax".

    - Clarified the statement of exercise 11.

3.  Consider the algorithm BEFORE thinking about programming language
    syntax and semantics.

4.  Consider using auxiliary functions to carry out part of the
    desired computation.

    For example, this might be useful in the "ccArea" and "validDay"
    exercises.

5.  Haskell "Bool" values can be compared for equality and
    inequality. We can use operators &&, ||, and "not" on "Bool"
    values.

6.  Conditions in Haskell guards and "if"-expression are just "Bool"
    values. They can involve calls to "Bool"-returning functions.

7.  The parameters in function headers can be patterns such as "0" or
    "True" to exactly match the given structures and values.

8.  Haskell integers support binary operations "min" and "max", which
    return the minimum and maximum, respectively, of their two
    arguments.

9.  It is probably sufficient to do testing interactively from ghci
    for this assignment. However, if you wish to build an ad hoc test
    script, you might want to look at the test script I did for the
    factorial functions from chapter 4 in the file TestFactorial.hs.
    This file is discussed in Chapter 12. It follows the testing
    concepts as discussed in Chapter 11.

-}


-- ELIFP Ch. 5, Ex. 2
prodSqSmall :: Double -> Double -> Double -> Double
prodSqSmall x y z
    | x >= y && x >= z = mulSq y z  -- x largest
    | y >= x && y >= z = mulSq x z  -- y largest
    | otherwise        = mulSq x y  -- z largest

-- To make prodSqSmall more readable
mulSq m n = m*m * n*n


-- ELIFP Ch. 5, Ex. 3
xor :: Bool -> Bool -> Bool
xor p q = p /= q

-- An alternative using pattern matching
xor2 :: Bool -> Bool -> Bool
xor2 True  False = True
xor2 False True  = True
xor2 _     _     = False

-- ELIFP Ch. 5, Ex. 4
implies :: Bool -> Bool -> Bool
implies p q = not p || q

-- An alternative using pattern matching, probably better in this case
implies2 :: Bool -> Bool -> Bool
implies2 False True = False
implies2 _     _    = True

-- ELIFP Ch. 5, Ex. 7
type Diameter = Double
type Area     = Double

-- Note: In this function, I assumed the diameters are nonnegative.
-- Because the problem description did not explicitly say that, I
-- should have taken that into account. I am not precisely sure what a
-- negative value for a diameter should mean. But it could result in a
-- negative result from ccArea because of the min/max comparison.
ccArea :: Diameter -> Diameter -> Area
ccArea x y = area ((max x y)/2) - area ((min x y)/2)

-- Alternative: Assume a negative diameter means the same as positive
ccArea2 :: Diameter -> Diameter -> Area
ccArea2 x y = area ((max x' y')/2) - area ((min x' y')/2)
    where x' = abs x
          y' = abs y
          
-- But as a student pointed out, we do not have to do min/max
ccArea3 :: Diameter -> Diameter -> Area
ccArea3 x y = abs (area (x/2) - area (y/2))
          
-- Area of circle with radius r
area r = pi * r^2


-- ELIFP Ch. 5, Ex. 9
-- addTax c p = ct
-- ct = c + (p/100) * c  OR  ct = (1 + (p/100)) * c
-- What if negative cost and/or percent? What should they mean?
type Cost    = Double
type Percent = Double

addTax :: Cost -> Percent -> Cost
addTax c p = c + (p/100) * c

-- c = ct / (1 + (p/100))
subTax :: Cost -> Percent -> Cost
subTax ct p = ct / (1 + (p/100))



-- ELIFP Ch. 5, Ex.11
-- Using proleptic Gregorian calendar: 1 BC is year 0, 2 BC is -1, ...
-- Proleptic Gregorian calendar underlies ISO 8601 standard

-- Introduce type aliases to help us keep straight
type Month = Int
type Day   = Int
type Year  = Int

validDay :: (Month,Day,Year) -> Bool
validDay (mon,day,yr) =
    validYr yr && validMon mon && validDayOfMon mon day yr 

validMon mon = 1 <= mon && mon <= 12

-- This version is using the proleptic Gregorian calendar, so all
-- years are valid. However, the exercise description allows this
-- to be restricted to "yr > 0".

validYr yr = True

validDayOfMon mon day yr = 1 <= day && day <= daysInMonth mon yr

-- daysInMonth month year
-- Precondition: month and year both in valid ranges
daysInMonth mon yr
    | mon == 4 || mon == 6 || mon == 9 || mon == 11
                            = 30
    | mon == 2              = daysInFeb yr
    | 1 <= mon && mon <= 12 = 31

daysInFeb yr
    | leapYr yr = 29
    | otherwise = 28

-- Using proleptic Gregorian calendar
leapYr yr =
    yr `rem` 4 == 0 && (yr `rem` 100 /= 0 || yr `rem` 400 == 0)


-- ELIFP Ch. 5, Ex. 12
-- Roman numerals in range [0-3999], where 0 is empty string
type Roman = String

roman :: Int -> Roman
roman n
    | n >= 4000 = error ("Above range: " ++ show n)
    | n <  0    = error ("Below range: " ++ show n)
    | otherwise = roman' n

-- Precondition: 0 <= n <= 3999
roman' n
    | n >= 1000 = "M"  ++ roman' (n-1000)
    | n >= 900  = "CM" ++ roman' (n-900)
    | n >= 500  = "D"  ++ roman' (n-500)
    | n >= 400  = "CD" ++ roman' (n-400)
    | n >= 100  = "C"  ++ roman' (n-100)
    | n >= 90   = "XC" ++ roman' (n-90)
    | n >= 50   = "L"  ++ roman' (n-50)
    | n >= 40   = "XL" ++ roman' (n-40)
    | n >= 10   = "X"  ++ roman' (n-10)
    | n == 9    = "IX"
    | n >= 5    = "V"  ++ roman' (n-5)
    | n == 4    = "IV"
    | n == 3    = "III"
    | n == 2    = "II"
    | n == 1    = "I"
    | n == 0    = ""   -- allowed by exercise description
