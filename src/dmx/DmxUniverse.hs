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
instance Show DmxUniverseBool where
  show (DmxUniverseBool bools) =
    let channelNumbers = unwords $ map (\(i, b) -> if not b then show i else "") $ zip [1..] bools
    in "Mismatched channel #'s:" ++ channelNumbers ++ "\nDmxUniverseBool: " ++ show bools
-- instance Show DmxUniverseBool where
--   show (DmxUniverseBool bools) =
--     let channelNumbers = unwords $ map (\(i, b) -> if not b then show i else "") $ zip [1..] bools
--         boolsStr = unwords $ map (\b -> if b then " " else "F") bools
--     in "Mismatched channels: " ++ channelNumbers ++ "\nDmxUniverseBool: " ++ boolsStr


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

stringToUniverse :: String -> Maybe DmxUniverse -> DmxUniverse
stringToUniverse s mUniverse =
  let tokens = words s
      chTokens = takeWhile (/= "at") tokens
      chRanges = parseChannelRanges chTokens
      level = parseLevel $ last tokens
      initialUniverse = maybe (DmxUniverse (replicate dmxUniverseSize 0)) id mUniverse
      updatedChannels = zipWith updateChannel [1 ..] (unDmxUniverse initialUniverse)
      updateChannel ch currentLevel = if ch `elem` chRanges then level else currentLevel
  in DmxUniverse updatedChannels

testStringToUniverse :: IO ()
testStringToUniverse = do
  let testCases = [ ( "7 to 323 at 10%"
                    , Just emptyDmxUniverse
                    , DmxUniverse $ replicate 6 0 ++ replicate 318 26 ++ replicate (dmxUniverseSize - (6 + 318)) 0
                    , "Passing Case"
                    )
                  , ( "8 to 386 at 10%"
                    , Just emptyDmxUniverse
                    , DmxUniverse $ replicate 6 0 ++ replicate 380 26 ++ replicate (dmxUniverseSize - (6 + 380)) 0
                    , "Failing Case"
                    )
                  ]

  let runTestCase (input, initialUniverse, expectedOutput, description) = do
        putStrLn $ "Running test: " ++ description
        let actualOutput = stringToUniverse input (initialUniverse)
        if actualOutput == expectedOutput
          then putStrLn "Test passed."
          else do
            putStrLn "Test failed:"
            putStrLn $ "Expected: " ++ show expectedOutput
            putStrLn $ "Actual: " ++ show actualOutput
            let comparison = zip (unDmxUniverse actualOutput) (unDmxUniverse expectedOutput)
            let dmxUniverseBool = [actual == expected | (actual, expected) <- comparison]
            let mismatchedChannels = [i | (i, isEqual) <- zip [1..] dmxUniverseBool, not isEqual]
            putStrLn $ "Mismatched channels: " ++ show mismatchedChannels
            putStrLn $ "DmxUniverseBool: " ++ show dmxUniverseBool

  mapM_ runTestCase testCases


compareUniverses :: DmxUniverse -> DmxUniverse -> DmxUniverseBool
compareUniverses (DmxUniverse expected) (DmxUniverse actual) =
  DmxUniverseBool $ zipWith (==) expected actual


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