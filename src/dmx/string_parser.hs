-- module Main where

import System.Random
import Data.Word
import DmxUniverse 

main :: IO ()
main = do
    putStrLn "Running test for stringToUniverse function..."
    testStringToUniverse
    putStrLn "All tests passed."