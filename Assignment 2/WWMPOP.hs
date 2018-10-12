import Data.Fixed


-- lineWidth :: Int
-- lineWidth = 34


type Price = Centi
type Name = String
type CartPrices = [(Name, Price)]

formatDollars :: Price -> String
formatDollars x = show (x / 100)



formatLine :: (Name, Price) -> String
formatLine (ns, p) = 
    let 
        lengthName = length ns
        priceF = formatDollars p
        lengthPrice = length priceF
        amountPeriod = 34 - (lengthName + lengthPrice)
        printR = concat(replicate amountPeriod ".")
    in
         ns ++ printR ++ priceF ++ "\n"

formatLines :: CartPrices -> String
formatLines [] = []
formatLines ((ns, p):nss) = formatLine(ns, p)
    -- let
    --     first = formatLine((ns, p))
    --     rest = formatLines(xss)
    -- in
    --     first ++ rest

    

