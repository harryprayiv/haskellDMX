{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Main (main) where

import Data.Maybe
import Data.Ix (Ix(..), bounds)
import Data.Array (Array, array)
import Data.Array.IArray (IArray, (!), listArray, rangeSize)


-- DMX Data Type Type and Instance
instance Ix Int => Ix (Array Int Int) where
  range (l,u) = [array (l,u) [(i,i) | i <- [l..u]]]
  index (l,u) i = i - l
  inRange (l,u) i = l <= i && i <= u
  bounds (l,u) = (l, u)

newtype DMX = DMX (Array Int Int) deriving (Eq, Ord, Ix, Read, Show)

mkDMX :: [Int] -> Maybe DMX
mkDMX xs
  | length xs == 512 && all (\x -> x >= 0 && x <= 255) xs = Just $ DMX $ listArray (0,511) xs
  | otherwise = Nothing

-- DMX Selection Data Type and Instance
instance Ix Int => Ix (Array Int Bool) where
  range (l,u) = [array (l,u) [(i,i) | i <- [l..u]]]
  index (l,u) i = if (l <= i && i <= u) then (u - i) `mod` (u - l + 1) else error "index out of bounds"
  inRange (l,u) i = l <= i && i <= u
  bounds (l,u) = (l, u)

newtype DMXSel = DMXSel (Array Int Bool) deriving (Eq, Ord, Ix, Read, Show)

mkDMXSel :: [Bool] -> Maybe DMXSel
mkDMXSel xs
  | length xs == 512 = Just $ DMXSel $ listArray (0,511) xs
  | otherwise = Nothing

modDmxChVals :: DMX -> DMXSel -> Int -> DMX
modDmxChVals (DMX d) (DMXSel s) v =
  let d' = array (bounds d) [(i, if s ! i then v else d ! i) | i <- range (bounds d)]
  in DMX d'

-- Create a valid DMX array and DMXSel array
myDMX :: Maybe DMX
myDMX = mkDMX [0..511]

myDMXSel :: Maybe DMXSel
myDMXSel = mkDMXSel $ replicate 512 True

-- main :: IO ()
-- main = case (myDMX, myDMXSel) of
--     (Just dmx, Just dmxSel) ->
--         let modifiedDM


main :: IO ()
main = case (myDMX, myDMXSel) of
    (Just dmx, Just dmxSel) ->
        let modifiedDMX = modDmxChVals dmx dmxSel 42
        in putStrLn (show modifiedDMX)
    _ -> putStrLn "Failed to create DMX or DMXSel array"