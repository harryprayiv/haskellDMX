module Main where

import System.Random
import Data.Word
import DmxUniverse -- assuming your module name is DmxUniverse.hs

main :: IO ()
main = do
  -- Create a random DmxUniverse and print its values
  universeA <- randomDmxUniverse
  -- putStrLn $ "Random universe: " ++ show (unDmxUniverse universeA)

  -- Create an empty DmxUniverse and print its values
  let universeB = emptyDmxUniverse
  -- putStrLn $ "Empty universe: " ++ show (unDmxUniverse universeB)

  -- Create a random DmxUniverseBool and print its values
  randomSelection <- randomDmxUniverseBool
  -- putStrLn $ "Random selection: " ++ show (unDmxUniverseBool randomSelection)

  -- Test the setDmxChVals function
  let chSel = DmxUniverseBool [True, False, True, False, False, True]
      chVals = DmxUniverse [100, 20, 30, 254, 50, 60]
      newVals = setDmxChVals chVals chSel 255
  putStrLn $ ""
  putStrLn $ ""
  putStrLn $ ""
  putStrLn $ "Testing!"

  putStrLn $ "Old values: " ++ show (unDmxUniverse chVals)
  putStrLn $ "Selection: " ++ show (unDmxUniverseBool chSel)
  putStrLn $ "New values: " ++ show (unDmxUniverse newVals)

  -- let uvOne = DmxUniverse [1, 0, 2, 3]
  --     uvTwo = DmxUniverse [253, 121, 64, 32]
  --     result = mapDmxUniverses uvOne '+' uvTwo
  -- putStrLn $ "UV One: " ++ show (unDmxUniverse uvOne)
  -- putStrLn $ "UV Two: " ++ show (unDmxUniverse uvTwo)
  -- putStrLn $ "Result: " ++ show (unDmxUniverse result)
  -- Result should contains: DmxUniverse {unDmxUniverse = [255, 0, 255, 62]}

