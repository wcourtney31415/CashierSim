module Main where

import Lib

main :: IO ()
main = go []

go shoppingCart = do
  lineInput <- getLine
  if lineInput == "done" then
    putStrLn "Done."
  else
    let
      appendedList = scanItem shoppingCart lineInput
      targetRowLength = lengthOfLongestElement appendedList
      itemToStringWithLength item = itemToString targetRowLength item
    in
    do
      clear
      mapM_ putStrLn $ map itemToStringWithLength appendedList
      putStrLn $ "Subtotal: " ++ (show $ subtotal appendedList)
      putStrLn $ "Total: " ++ (show $ total appendedList)
      go appendedList


--clear = putStr "\ESC[1J"
clear = mapM_ putStrLn $ take 100 $ repeat ""
