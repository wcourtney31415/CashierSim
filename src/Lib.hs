module Lib where

data Barcode = String

data Item = Item
  {
  name :: String
  , price :: Double
  , barcode :: String
  , taxable :: Bool
  } deriving (Show)

salesTax = 0.07

inventory = [
    Item {name="Learn You a Haskell for Great Good!", price=44.95, barcode="689145728392", taxable = True}
    , Item {name="m&m's Minis", price=1.30, barcode="040000482239", taxable = True}
    , Item {name="Extra Gum", price=0.99, barcode="02289805", taxable = True}
    , Item {name="Lottery Ticket", price=2.00, barcode="22350064411740", taxable = False}
  ]

lengthOfLongestElement :: [Item] -> Int
lengthOfLongestElement shoppingCart =
  let

    getRowLength item =
      let
        strName = name item
        strPrice = show $ price item
        minimumDots = 5
        dots = take 5 $ repeat '.'
        line = strName ++ dots ++ strPrice
      in
        length line

    arr = map getRowLength shoppingCart
    lineLength = foldl1 (max) arr
  in
    lineLength

subtotal shoppingCart = foldl (+) 0 $ map (price) shoppingCart

total shoppingCart = subtotal shoppingCart * (salesTax + 1)

itemToString lineLength item =
  let
    itemName = name item
    itemPrice = show $ price item
    lengthBeforeDots = length $ itemName ++ itemPrice
    dotsNeeded = lineLength - lengthBeforeDots
    dots = take dotsNeeded $ repeat '.'
    ret = itemName ++ dots ++ itemPrice
  in
    ret


scanItem shoppingCart itemBarcode =
  let
    scannedItem = head [x | x <- inventory, barcode x == itemBarcode]
  in
    shoppingCart ++ [scannedItem]
