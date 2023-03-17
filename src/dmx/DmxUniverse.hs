module DmxUniverse (
    DmxUniverse(..),
    DmxUniverseBool(..),
    dmxUniverseSize,
    dmxUniverseBoolSize,
    setDmxChVals,
    modifyDmxChVals,
    stringToUniverse,
    newDmxUniverse,
    emptyDmxUniverse,
    emptyDmxUniverseBool,
    randomDmxUniverse,
    randomDmxUniverseBool,
    testStringToUniverse
) where

import System.Random
import Data.Word
import Data.Char (isDigit)

-- newtype DmxUniverse = DmxUniverse { unDmxUniverse :: [Word8] }
newtype DmxUniverse = DmxUniverse { unDmxUniverse :: [Word8] }
  deriving (Eq, Show)


newtype DmxUniverseBool = DmxUniverseBool { unDmxUniverseBool :: [Bool] }

dmxUniverseSize :: Int
dmxUniverseSize = 512

dmxUniverseBoolSize :: Int
dmxUniverseBoolSize = 512

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


setDmxChVals :: DmxUniverse -> DmxUniverseBool -> Word8 -> DmxUniverse
setDmxChVals (DmxUniverse chVals) (DmxUniverseBool chSel) v =
  let newChVals = zipWith (\sel val -> if sel then v else val) chSel chVals
  in DmxUniverse newChVals

percentageToWord8 :: String -> Word8
percentageToWord8 s = round (255 * (read s / 100))

parseLevel :: String -> Word8
parseLevel lvlStr
  | lvlStr == "full" = 255
  | lvlStr == "half" = 128
  | lvlStr == "off" = 0
  | last lvlStr == '%' =
      let numStr = init lvlStr
      in case reads numStr of
          [(num, "")] | num >= (0 :: Int) && num <= 100 ->
            round $ fromIntegral num * 255 / 100
          _ -> error $ "Invalid percentage: " ++ lvlStr
  | [(lvl, "")] <- reads lvlStr, lvl <= (255 :: Int) = fromIntegral lvl
  | otherwise = error $ "Invalid level: " ++ lvlStr

parseChannelRanges :: [String] -> [Int]
parseChannelRanges [] = []
parseChannelRanges (x:xs)
    | all isDigit x =
        let start = read x
        in case xs of
            (y : z : ys) | y `elem` ["to", "through"] && all isDigit z ->
                let end = read z
                in [start .. end] ++ parseChannelRanges ys
            _ -> start : parseChannelRanges xs
    | x == "and" = parseChannelRanges xs
    | otherwise = error "Invalid channel specifier"

parseValue :: [String] -> Word8
parseValue ["full"] = 255
parseValue (x:_) | all isDigit x = read x
parseValue _ = error "Invalid value specifier"

stringToUniverse :: String -> DmxUniverse
stringToUniverse s =
  let tokens = words s
      (chStart, chEnd) =
        case tokens of
          [startStr, "to"     , endStr, "at"     , lvlStr] -> parseRange startStr endStr
          [startStr, "through", endStr, "at"     , lvlStr] -> parseRange startStr endStr
          [startStr, "and"    , endStr, "at"     , lvlStr] -> parseRange startStr endStr
          [startStr, "to"     , endStr, "@"      , lvlStr] -> parseRange startStr endStr
          [startStr, "through", endStr, "@"      , lvlStr] -> parseRange startStr endStr
          [startStr, "and"    , endStr, "@"      , lvlStr] -> parseRange startStr endStr
          _ -> error $ "Invalid command: " ++ s
      parseRange startStr endStr =
        let [(start, "")] = reads startStr
            [(end, "")] = reads endStr
        in (min start end, max start end)
      level = parseLevel $ last tokens
      selectedChannels = [1 .. chEnd] ++ replicate (dmxUniverseSize - chEnd) 0
      updatedChannels =
        map (\ch -> if ch >= chStart && ch <= chEnd then level else 0) selectedChannels
  in DmxUniverse updatedChannels

testStringToUniverse :: IO ()
testStringToUniverse = do
    let testInput = "6 to 36 at full"
    let expectedOutput = DmxUniverse $ replicate 6 0 ++ replicate 36 255 ++ replicate (dmxUniverseSize - (36 + 6)) 0
    let actualOutput = stringToUniverse testInput
    if actualOutput == expectedOutput
        then putStrLn "stringToUniverse test passed."
        else putStrLn $ "stringToUniverse test failed: expected " ++ show expectedOutput ++ " but got " ++ show actualOutput


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