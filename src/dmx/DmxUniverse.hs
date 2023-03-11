module DmxUniverse (
    DmxUniverse(..),
    DmxUniverseBool(..),
    dmxUniverseSize,
    dmxUniverseBoolSize,
    emptyDmxUniverse,
    emptyDmxUniverseBool,
    randomDmxUniverse,
    randomDmxUniverseBool,
    newDmxUniverse,
    setDmxChVals
) where

import System.Random
import Data.Word

newtype DmxUniverse = DmxUniverse { unDmxUniverse :: [Word8] }
newtype DmxUniverseBool = DmxUniverseBool { unDmxUniverseBool :: [Bool] }

dmxUniverseSize :: Int
dmxUniverseSize = 512

dmxUniverseBoolSize :: Int
dmxUniverseBoolSize = 512

emptyDmxUniverse :: DmxUniverse
emptyDmxUniverse = DmxUniverse $ replicate dmxUniverseSize 0

emptyDmxUniverseBool :: DmxUniverseBool
emptyDmxUniverseBool = DmxUniverseBool $ replicate dmxUniverseBoolSize True

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

setDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
setDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  in DmxUniverse newChVals
