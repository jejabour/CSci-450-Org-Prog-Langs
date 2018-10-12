import Data.Fixed


-- lineWidth :: Int
-- lineWidth = 34


type Price = Centi
type Name = String
type CartPrices = [(Name, Price)]

formatDollars :: Price -> String
formatDollars x = show (x / 100)

getLengthName :: Name -> Int
getLengthName n = length n

getLengthPrice :: Price -> Int
getLengthPrice p = 
    let 
        getFormatPrice = formatDollars p
        lengthPrice = length getFormatPrice
    in 
        id lengthPrice

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
formatLines :: CartPrices -> String
formatLines [] = []
formatLines ((ns, p):nss) = formatLine(ns, p)


calcSubtotal :: CartPrices -> Price
calcSubtotal ((ns, p):nss) = p + calcSubtotal(nss)

