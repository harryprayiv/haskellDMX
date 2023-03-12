module Main where

import System.Random
import Data.Word
import DmxUniverse 
import DmxUniverseBuilders 


main :: IO ()
main = do
  -- Create a random DmxUniverse and print its values
  universeA <- randomDmxUniverse
  putStrLn $ "Universe A (Random): " ++ show (unDmxUniverse universeA)
  putStrLn "\n\n\n"

  -- Create an empty DmxUniverse and print its values
  -- let universeB = emptyDmxUniverse
  -- putStrLn $ "Universe B (Empty): " ++ show (unDmxUniverse universeB)
  -- putStrLn "\n\n\n"

  -- Create a random DmxUniverseBool and print its values
  randomSelection <- randomDmxUniverseBool
  putStrLn $ "Random Selection: " ++ show (unDmxUniverseBool randomSelection)
  putStrLn "\n\n\n"

  -- -- Test the setDmxChVals function
  -- let newVals = setDmxChVals universeA randomSelection 69
  --     newRandVals = setDmxChVals universeB randomSelection 255

  -- putStrLn $ "Universe A Transformed using only randomSelection cells to 69: " ++ show (unDmxUniverse newVals)
  -- putStrLn "\n\n\n"

  -- putStrLn $ "Universe B Transformed using only randomSelection cells to 2: " ++ show (unDmxUniverse newRandVals)
  -- putStrLn "\n\n\n"
  -- let
  --   modifiedUniverse = modifyDmxChVals universeA randomSelection 69 '+'
  -- putStrLn $ "A + 69 if selected: " ++ show (unDmxUniverse modifiedUniverse) 
  -- putStrLn "\n\n\n"
  -- let
  --   modifiedUniverse = modifyDmxChVals universeB randomSelection 20 '+'
  -- putStrLn $ "B + 20 if selected: " ++ show (unDmxUniverse modifiedUniverse)
  -- putStrLn "\n\n\n"
  let
    modifiedUniverse = modifyDmxChVals universeA randomSelection 2 '*'
  putStrLn $ "A * 20 if selected: " ++ show (unDmxUniverse modifiedUniverse) 
