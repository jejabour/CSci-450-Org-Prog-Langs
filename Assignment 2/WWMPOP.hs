import Data.Fixed


-- lineWidth :: Int
-- lineWidth = 34

-- Centi comes from the import, and makes it so it always
-- has two decimal points
type Price = Centi
type Name = String
type CartPrices = [(Name, Price)]

-- Divides the given number by 100 to make cents out
-- of the given number
formatDollars :: Price -> String
formatDollars x = show (x / 100)

-- Returns the length of the Name
getLengthName :: Name -> Int
getLengthName n = length n

-- Returns the length of the Price
getLengthPrice :: Price -> Int
getLengthPrice p = 
    let 
        getFormatPrice = formatDollars p
        lengthPrice = length getFormatPrice
    in 
        id lengthPrice

-- Returns the name of the item, follwed by a number of "."'s
-- follwed by the price, where the number of characters is always
-- 34
formatLine :: (Name, Price) -> String
formatLine (ns, p) = 
    let 
        priceFormat = formatDollars p
        lengthName = getLengthName ns
        lengthPrice = getLengthPrice p
        amountPeriod = 34 - (lengthName + lengthPrice)
        printR = concat(replicate amountPeriod ".")
    in
         ns ++ printR ++ priceFormat ++ "\n"


-- No idea if this is working
-- Creates a list of all the items using formatLine
formatLines :: CartPrices -> String
formatLines [] = []
formatLines ((ns, p):nss) = formatLine(ns, p)

-- No idea if this is working
-- Sifts through the formatLines to find the total of each Price
calcSubtotal :: CartPrices -> Price
calcSubtotal ((ns, p):nss) = p + calcSubtotal(nss)

