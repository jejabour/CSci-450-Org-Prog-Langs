{- CSci 450/503: Org. of Programming Languages, Seq Testing Module
   Fall 2017, Examination #2, Problems 10(b), Testing for 11
   H. Conrad Cunningham

1234567890123456789012345678901234567890123456789012345678901234567890

2017-10-19: Testing separated from SeqOps module
2017-10-21: Comments updated to reflect functions included on exam,
            Improved testing of some functions

This module tests all 8 functions given in the problem #11 description
(lenSeq, isNil, tailSeq, appSeq, revSeq, mapSeq, eqSeq, listToSeq).
It also tests several items not used in the Exam: functions headSeq,
seqToList, filterSeq, foldrSeq, and printSeq and the Eq instance
declaration.

This uses functions listToSeq and seqToList in testing other
functions.

-}

module SeqTest -- problem 10(b) "module" thru "import", name SeqUser
where

import SeqOps

pass True  = "SUCCESS"
pass False = "FAIlURE"

main = do
    -- Define test data
    let nil = Nil :: Seq Int  -- force element type
    let emp = [] :: [Int]
    let t1  = Cons 5 (Cons 4 (Cons 4 (Cons 7 nil)))
    let t1' = Cons 4 (Cons 4 (Cons 7 nil))
    let t1R = Cons 7 (Cons 4 (Cons 4 (Cons 5 nil)))
    let t2  = [5,4,4,7]
    let t2' = tail t2 
    let t2R = [7,4,4,5]
    let t3  = Cons 1 (Cons 0 (Cons 0 (Cons 3 nil)))
    let t4  = [1,0,0,3]
    let t5  = Cons 3 nil
    let t6  = [3]

    -- Print test data
    putStrLn "\nTest Data"
    putStr "Sequence nil:  "
    putStrLn $ show nil
    putStr "Sequence t1:   "
    putStrLn $ show t1
    putStr "Sequence t1':  "
    putStrLn $ show t1'
    putStr "Sequence t1R:  "
    putStrLn $ show t1R
    putStr "List t2:       "    
    putStrLn $ show $ t2
    putStr "List t2':      "    
    putStrLn $ show $ t2'
    putStr "List t2R:      "    
    putStrLn $ show $ t2R
    putStr "Sequence t3:   "
    putStrLn $ show t3
    putStr "List t4:       "
    putStrLn $ show t4
    putStr "Sequence t5:   "
    putStrLn $ show t5
    putStr "List t6:       "
    putStrLn $ show t6

    -- Test isNil
    putStrLn "\n Test isNil"
    putStr "isNil Nil == True:        "
    putStrLn $ show $ pass $ isNil Nil == True
    putStr "isNil t5 == False:        "
    putStrLn $ show $ pass $ isNil t5 == False
    putStr "isNil t1 == False:        "
    putStrLn $ show $ pass $ isNil t1 == False

    -- Test seqToList and listToSeq
    putStrLn "\nTest seqToList and listToSeq"
    putStr "seqToList nil == emp:       "
    putStrLn $ show $ pass $ seqToList nil == emp
    putStr "seqToList t1 == t2:        "
    putStrLn $ show $ pass $ seqToList t1 == t2
    putStr "seqToList t5 == t6:        "
    putStrLn $ show $ pass $ seqToList t5 == t6
    putStr "(seqToList $ listToSeq emp) == emp:  "
    putStrLn $ show $ pass $ (seqToList $ listToSeq emp) == emp
    putStr "(seqToList $ listToSeq t2) == t2:  "
    putStrLn $ show $ pass $ (seqToList $ listToSeq t2) == t2

    -- Test headSeq and tailSeq
    putStrLn "\nTest headSeq and tailSeq"
    putStr "headSeq t1 == 5:           "
    putStrLn $ show $ pass $ headSeq t1 == 5
    putStr "tailSeq t1 == t1':         "
    putStrLn $ show $ pass $ tailSeq t1 == t1'

    -- Test lenSeq
    putStrLn "\nTest lenSeq"
    putStr "lenSeq nil == 0:           "
    putStrLn $ show $ pass $ lenSeq nil == 0
    putStr "lenSeq t1 == 4:            "
    putStrLn $ show $ pass $ lenSeq t1 == 4
    
    -- Test revSeq
    putStrLn "\nTest revSeq"
    putStr "(seqToList $ revSeq nil) == emp:  "
    putStrLn $ show $ pass $ (seqToList $ revSeq nil) == emp
    putStr "(seqToList $ revSeq t1) == t2R:  "
    putStrLn $ show $ pass $ (seqToList $ revSeq t1) == t2R
    putStrLn ""

    -- Test eqSeq
    putStr "eqSeq nil nil == True:      "
    putStrLn $ show $ pass $ eqSeq nil nil == True
    putStr "eqSeq t1 t1 == True:        "
    putStrLn $ show $ pass $ eqSeq t1 t1 == True
    putStr "eqSeq t1 t1' == False:      "
    putStrLn $ show $ pass $ eqSeq t1 t1' == False
    putStr "eqSeq Nil t1 == False:      "
    putStrLn $ show $ pass $ eqSeq Nil t1 == False
    putStr "eqSeq t1 Nil == False:      "
    putStrLn $ show $ pass $ eqSeq t1 Nil == False
    putStrLn ""

    -- Test appSeq
    putStr "eqSeq t1 $ appSeq t1 Nil == True:     "
    putStrLn $ show $ pass $ (eqSeq t1 $ appSeq t1 Nil) == True
    putStr "(eqSeq t1 $ appSeq Nil t1) == True:   "
    putStrLn $ show $ pass $ (eqSeq t1 $ appSeq Nil t1) == True
    putStr "(eqSeq t1 $ appSeq t5 t1') == False:  "
    putStrLn $ show $ pass $ (eqSeq t1 $ appSeq t5 t1') == False
    putStrLn ""

    -- Test mapSeq
    putStr "(eqSeq Nil $ mapSeq (flip (-) 4) Nil) == True:  "
    putStrLn $ show $ pass $
               (eqSeq Nil $ mapSeq (flip (-) 4) Nil) == True
    putStr "(eqSeq t3 $ mapSeq (flip (-) 4) t1) == True:    "
    putStrLn $ show $ pass $
               (eqSeq t3 $ mapSeq (flip (-) 4) t1) == True
    putStrLn ""

    -- Test filterSeq
    putStr "(eqSeq Nil $ filterSeq (>=3) Nil) == True:      "
    putStrLn $ show $ pass $
               (eqSeq Nil $ filterSeq (>=3) Nil) == True
    putStr "(eqSeq t5 $ filterSeq (>=3) == True:            "
    putStrLn $ show $ pass $
        (eqSeq t5 $ filterSeq (>=3) t3) == True
    putStrLn ""

    -- Test foldrSeq
    putStr "foldrSeq (+) 0 nil == 0      "
    putStrLn $ show $ pass $ foldrSeq (+) 0 nil == 0
    putStr "foldrSeq (+) 0 t1 == 20:     "
    putStrLn $ show $ pass $ foldrSeq (+) 0 t1 == 20
    putStrLn ""

    -- Test (==) 
    putStr "(Nil == Nil) == True:        "
    putStrLn $ show $ pass $ (nil == nil) == True
    putStr "(t1 == t1) == True:          "
    putStrLn $ show $ pass $ (t1 == t1) == True
    putStr "(t1 == Nil) == False:        "
    putStrLn $ show $ pass $ (t1 == Nil) == False

    -- Test printSeq
    printSeq t1
    putStrLn "\nEnd of Testing\n"
