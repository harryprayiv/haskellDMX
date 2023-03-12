module DmxUniverse (
    DmxUniverse(..),
    DmxUniverseBool(..),
    dmxUniverseSize,
    dmxUniverseBoolSize,
    setDmxChVals,
    modifyDmxChVals) 
where

import System.Random
import Data.Word

newtype DmxUniverse = DmxUniverse { unDmxUniverse :: [Word8] }
newtype DmxUniverseBool = DmxUniverseBool { unDmxUniverseBool :: [Bool] }

dmxUniverseSize :: Int
dmxUniverseSize = 512

dmxUniverseBoolSize :: Int
dmxUniverseBoolSize = 512

setDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
setDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  in DmxUniverse newChVals

modifyDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> Char -> DmxUniverse
modifyDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v op =
  let newChVals = zipWith (\sel val -> case op of
                                         '+' -> if sel then clamp (fromIntegral val + fromIntegral v) else val
                                         '-' -> if sel then clamp (fromIntegral val - fromIntegral v) else val
                                         '*' -> if sel then clamp (fromIntegral val * fromIntegral v) else val
                                         '/' -> if sel then clamp (if v == 0 then fromIntegral val else fromIntegral val `div` fromIntegral v) else val
                                         _   -> val) chSel chVals
  in DmxUniverse newChVals
  where
    clamp x = fromIntegral $ max 0 $ min 255 x




{- 
-- (+) -- mathOps '+' a b = max 255 (a + b)
incrDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
incrDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  -- let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  -- in DmxUniverse newChVals

-- (-) -- mathOps '-' a b = min 0 (a - b)
decrDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
decrDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  -- let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  -- in DmxUniverse newChVals

-- (*) -- mathOps '*' a b = max 255 (a * b)
multDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
multDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  -- let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  -- in DmxUniverse newChVals

-- (/) -- mathOps '/' a b = a `div` b
divDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
divDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  -- let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  -- in DmxUniverse newChVals
-- mathOps '/' _ 0 = 0
 -}


-- Broken flexible math option
-- -- Define mathOps function for mathematical operations
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