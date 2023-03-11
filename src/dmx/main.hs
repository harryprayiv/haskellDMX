module Main where

import System.Random
import Data.Word
import DmxUniverse 

main :: IO ()
main = do
  -- Create a random DmxUniverse and print its values
  universeA <- randomDmxUniverse
  putStrLn $ "Universe A: " ++ show (unDmxUniverse universeA)
  putStrLn "\n\n\n"

  -- Create an empty DmxUniverse and print its values
  let universeB = emptyDmxUniverse
  putStrLn $ "Universe B: " ++ show (unDmxUniverse universeB)
  putStrLn "\n\n\n"

  -- Create a random DmxUniverseBool and print its values
  randomSelection <- randomDmxUniverseBool
  putStrLn $ "Random selection: " ++ show (unDmxUniverseBool randomSelection)
  putStrLn "\n\n\n"

  -- Test the setDmxChVals function
  let newVals = setDmxChVals universeB randomSelection 69
      newRandVals = setDmxChVals universeA randomSelection 2

  putStrLn $ "New values: " ++ show (unDmxUniverse newVals)
  putStrLn "\n\n\n"

  putStrLn $ "New values: " ++ show (unDmxUniverse newRandVals)
