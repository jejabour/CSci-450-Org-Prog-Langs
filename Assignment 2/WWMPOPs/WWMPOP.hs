import Data.Fixed


-- lineWidth :: Int
-- lineWidth = 34

-- Centi comes from the import, and makes it so it always
-- has two decimal points
type Price = Centi
type Name = String
type BarCode = Int
type CartItems = [BarCode]
type CartPrices = [(Name, Price)]
type Bill = (CartPrices, Price, Price, Price)
type PriceList = [(BarCode, Name, Price)]

dataBase :: PriceList
dataBase = [(1848, "Vanilla yogurt cups (4)",    188),
            (1620, "Ground turkey (1 lb)",       316), 
            (1492, "Corn flakes cereal",         299), 
            (1773, "Black tea bags (100)",       307), 
            (2525, "Athletic socks (6)",         825), 
            (9595, "Claw hammer",                788), 
            (1945, "32-in TV",                 13949), 
            (1066, "Zero sugar cola (12)",       334),
            (2018, "Haskell programming book",  4495)
            ]

taxRate :: Double
taxRate = 0.07

-- priceCart :: PriceList -> CartItems -> CartPrices
-- priceCart [] = 0
-- priceCart names prices =
--     | null hit = ("Unknown ", 0)
--     | otherwise = head hit
--         where hit = [(n, p) | (b, n, p) <- dataBase, b == barCode]


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
formatLines ((ns, p):nss) = 
    let
        first = formatLine((ns, p))
        rest = formatLines(nss)
    in
        first ++ rest

-- No idea if this is working
-- Sifts through the formatLines to find the total of each Price
-- calcSubtotal :: CartPrices -> Price
-- calcSubtotal ((ns, p):nss) = p + calcSubtotal(nss)
-- calcSubtotal [] = 0
-- calcSubtotal ((xs,y):xss) = 
--     let
--         getNum = y + calcSubtotal(xss)
--     in
--         id getNum ++ taxRate


formatAmt :: String -> Price -> String
formatAmt a b = "\n" ++ formatLine (a, b)

formatBill :: Bill -> String
formatBill [] = []
formatBill ((xs, y):xss) = 
    let
        line = formatLine ((xs, y):xss)
        getTotal = calcSubtotal ((xs, y):xss)
        total = formatAmt getTotal
    in
        line ++ "total"



