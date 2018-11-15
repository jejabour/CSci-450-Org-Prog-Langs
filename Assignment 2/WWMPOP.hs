-- Joseph Jabour, CSci 450 Organization of Programming Languages with Dr. Cunningham
-- Assignment 2
-- Citations
-- I absolutely got the look function from a powerpoint I found online because I had no idea
-- how to do that. Here's a link to it
-- 
-- https://sandcastle.cosc.brocku.ca/~mwinter/Courses/4P41/Week3.ppt
-- 
-- I saw a couple github repositories online but I tried to not use their code for
--  obvious reasons
-- However, when I was trying to find out how to just sum the second part of a tuple for 
-- the calcSubTotal, I came across another github repo that had exactly what I needed, and it
-- was such a short and easy function I didn't think it'd hurt to use that one, since it was just
-- the function I needed
-- Here's the repo
-- https://gist.github.com/sjoness/6183027
-- 
-- The import is for the Centi data type, which I found on some stack overflow page about
-- keeping 2 decimals. I don't know where the page is now, but the Data.Fixed Centi is in 
-- Haskell api so I don't think I need to cite that
-- I'm honestly not totally sure it works, I couldn't figure out how to test it with a big
-- receipt order, but I'm pretty sure it works. All the examples from the instructions 
-- seemed like they work here, except I cannot for the life of me figure out how to make
-- new lines show up in ghci terminal, and I couldn't figure out why my formatAmt kept
-- putting slashes around it's stuff. Other than that, I think the rest is my own fault


import Data.Fixed

type BarCode = Int
type Price = Centi
type Name = String

type CartItems = [BarCode]
type CartPrices = [(Name, Price)]

type Bill = (CartPrices, Price, Price, Price)
type PriceList = [(BarCode, Name, Price)]

taxRate :: Centi
taxRate = 0.07

database :: PriceList
database = [ (1848, "Vanilla yogurt cups (4)",    188),
                (1620, "Ground turkey (1 lb)",       316), 
                (1492, "Corn flakes cereal",         299), 
                (1773, "Black tea bags (100)",       307), 
                (2525, "Athletic socks (6)",         825), 
                (9595, "Claw hammer",                788), 
                (1945, "32-in TV",                 13949), 
                (1066, "Zero sugar cola (12)",       334),
                (2018, "Haskell programming book",  4495)
            ]

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


-- Creates a list of all the items using formatLine
formatLines :: CartPrices -> String
formatLines [] = []
formatLines ((ns, p):nss) = 
    let
        var1 = formatLine((ns, p))
        vartotal = formatLines(nss)
    in
        var1 ++ vartotal


-- Sifts through the formatLines to find the total of each Price
calcSubtotal :: CartPrices -> Price
calcSubtotal cart = sum price
        where
            (_, price) = unzip cart


-- Takes a string and a price and formats them, such as Total 10000 = Total........100.00
formatAmt :: String -> Price -> String
formatAmt str price = formatLine (str, price)


-- Makes the string for the end of the receipt, such as the subtotal, tax, and total
formatBill :: Bill -> String
formatBill (_, subtot, tax, tot) = show subtotF ++ show taxF ++ show totF
            where
                subtotF = formatAmt "Subtotal" subtot
                taxF = formatAmt "Tax" tax
                totF = formatAmt "Total" tot


-- Finds an item from the database based on its barcode
look :: PriceList -> BarCode -> (Name, Price)
look db b = if res == [] then ("Unknown Item",0) else head res
    where res = [ (n,p) | (bn,n,p) <- db, b == bn ]


-- Uses the look function
lookup' :: BarCode -> (Name, Price)
lookup' = look database


-- This goes through the database and finds an item based on it's bar code
priceCart :: PriceList -> CartItems -> CartPrices
priceCart prices cart = map(look prices) cart


-- This is the math behind the end bill stuff
makeBill :: CartPrices -> Bill
makeBill cart =
    let 
        subtot = calcSubtotal cart
        tax = subtot * taxRate
        tot = subtot + tax
    in
        id (cart, subtot, tax, tot)


-- This function does the entire thing
makeReceipt :: PriceList -> CartItems -> String
makeReceipt db cart = 
    let
        itemF = priceCart db cart
        items = formatLines itemF
        endbill = makeBill itemF
        endBillS = formatBill endbill
    in 
        items ++ endBillS

