module Main (main) where

import Data.Array

-- DMX Data Type
newtype DMX = DMX (Array Int Int)

mkDMX :: [Int] -> Maybe DMX
mkDMX xs
  | length xs == 512 && all (\x -> x >= 0 && x <= 255) xs = Just $ DMX $ listArray (0,511) xs
  | otherwise = Nothing

-- DMX Selection Data Type
newtype DMXSel = DMXSel (Array Int Bool)

mkDMXSel :: [Bool] -> Maybe DMXSel
mkDMXSel xs
  | length xs == 512 = Just $ DMXSel $ listArray (0,511) xs
  | otherwise = Nothing

-- Modify DMX channel values
modDmxChVals :: DMX -> DMXSel -> Int -> DMX
modDmxChVals (DMX d) (DMXSel s) v =
  let d' = array (bounds d) [(i, if s ! i then v else d ! i) | i <- range (bounds d)]
  in DMX d'

-- Create a valid DMX array and DMXSel array
myDMX = mkDMX [0..511]
myDMXSel = mkDMXSel $ replicate 512 True

main = let myModifiedDMX = modDmxChVals (fromJust myDMX) (fromJust myDMXSel) 42
        in print myModifiedDMX
