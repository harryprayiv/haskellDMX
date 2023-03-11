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
    setDmxChVals,
    -- mathOps,
    -- mapDmxUniverses
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

-- Define mathOps function for mathematical operations
-- mathOps :: Char -> Word8 -> Word8 -> Word8
-- mathOps '+' a b = max 255 (a + b)
-- mathOps '-' a b = min 0 (a - b)
-- mathOps '*' a b = max 255 (a * b)
-- mathOps '/' _ 0 = 0
-- mathOps '/' a b = a `div` b

-- -- Define mapDmxUniverses function to apply mathematical operation
-- mapDmxUniverses :: DmxUniverse -> Char -> DmxUniverse -> DmxUniverse
-- mapDmxUniverses (DmxUniverse a) op (DmxUniverse b) =
--   DmxUniverse $ zipWith (mathOps op) a b