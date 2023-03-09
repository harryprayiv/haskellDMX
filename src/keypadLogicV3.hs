--_____________DMX Data Type____________--
newtype DMX = DMX (Array Int Int)
newtype DMXSel = DMXSel (Array Int Bool)

mkDMX :: [Int] -> Maybe DMX
mkDMX xs
  | length xs == 512 && all (\x -> x >= 0 && x <= 255) xs = Just $ DMX $ listArray (0,511) xs
  | otherwise = Nothing

--__________DMX Selection Data Type________________--
mkDMXSel :: [Bool] -> Maybe DMXSel
mkDMXSel xs
  | length xs == 512 = Just $ DMXSel $ listArray (0,511) xs
  | otherwise = Nothing


modDmxChVals :: DMX -> DMXSel -> Int -> DMX
modDmxChVals (DMX d) (DMXSel s) v =
  let d' = array (bounds d) [(i, if s ! i then v else d ! i) | i <- range (bounds d)]
  in DMX d'

-- Create a valid DMX array and DMXSel array
Just myDMX = mkDMX [0..511]

Just myDMXSel = mkDMXSel $ replicate 512 True

-- Create an invalid DMX array and an invalid DMXSel array
myInvalidDMX = mkDMX [0..511] ++ [999]

myInvalidDMXSel = mkDMXSel $ replicate 511 True

-- Use pattern matching to extract the underlying array from a DMX value 
-- then extract the underlying array from a DMXSel value
DMX arr = myDMX

DMXSel arr = myDMXSel

-- Modify the DMX array using modDmxChVals
let myModifiedDMX = modDmxChVals myDMX myDMXSel 42

-- Use pattern matching to extract the underlying array from a DMX value
DMX arr = myModifiedDMX









