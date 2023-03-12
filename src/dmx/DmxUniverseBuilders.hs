module DmxUniverseBuilders (
    newDmxUniverse,
    emptyDmxUniverse,
    emptyDmxUniverseBool,
    randomDmxUniverse,
    randomDmxUniverseBool,) 
where

import System.Random
import Data.Word
import DmxUniverse

emptyDmxUniverse :: DmxUniverse
emptyDmxUniverse = DmxUniverse $ replicate dmxUniverseSize 0

emptyDmxUniverseBool :: DmxUniverseBool
emptyDmxUniverseBool = DmxUniverseBool $ replicate dmxUniverseBoolSize False

randomDmxUniverse :: IO DmxUniverse
randomDmxUniverse = do
  gen <- newStdGen
  let values = take dmxUniverseSize $ randoms gen
  return $ DmxUniverse values

randomDmxUniverseBool :: IO DmxUniverseBool
randomDmxUniverseBool = do
  gen <- newStdGen
  let values = take dmxUniverseBoolSize $ randoms gen
  return $ DmxUniverseBool values

newDmxUniverse :: [Word8] -> Maybe DmxUniverse
newDmxUniverse bytes
  | length bytes /= dmxUniverseSize = Nothing
  | otherwise = Just $ DmxUniverse bytes
