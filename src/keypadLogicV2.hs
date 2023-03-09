--chat GPT's second attempt: 
-- Here is an updated version of the code with a few changes:

-- Removed unnecessary let bindings and used guards to simplify the code
-- Replaced pattern matching on Char values with a lookup table using Data.Map module
-- Removed unused variables and trailing dots
-- Corrected a couple of syntax errors in the code
{-# LANGUAGE NamedFieldPuns #-}

import Data.List (foldl')
import qualified Data.Map.Strict as Map



analogFaders :: Int
analogFaders = 9

analogFaderMap :: [Int]
analogFaderMap = [1, 2, 3, 4, 5, 6, 7, 8, 9] 

dmxChannels :: Int
dmxChannels = 512

dmxVal :: [Byte]
dmxVal = replicate dmxChannels 0

dmxSelection :: [Bool]
dmxSelection = replicate dmxChannels False

channelMap :: Map.Map Int Int
channelMap = Map.empty --global state Haskell no-no (will it always be empty?)

scalerVal :: Float

data SelectionMode = SingleChannel | And | Through | None deriving (Show, Eq)

-- data SelectionMode = None | SingleChannel | And | Through deriving (Show, Eq)

selectionType :: SelectionMode
selectionType = None --global state Haskell no-no

data PgmMode = FaderMode | KpdMode | KpdFaderMode deriving (Show, Eq)

controlMode :: PgmMode
controlMode = FaderMode --global state Haskell no-no

modeChosen :: Bool
modeChosen = False --global state Haskell no-no

data DisplayMode = Pockonsoled | SerialDisplay deriving (Show, Eq)

display :: DisplayMode
display = Pockonsoled

data KpdProgress = ModeSelect | NoCmd | DmxChOne | DmxChTwo | DmxIntensity deriving (Show, Eq)

kpdState :: KpdProgress
kpdState = ModeSelect

-- Keypad constants
rows :: Int
rows = 4

cols :: Int
cols = 4

pgmModeSelectionInt :: Int
pgmModeSelectionInt = 0

chOneKpdChar :: [Char]
chOneKpdChar = replicate 5 ' '

channelOneInt :: Int
channelOneInt = 0

intCount :: Int
intCount = 0

chTwoKpdChar :: [Char]
chTwoKpdChar = replicate 5 ' '

channelTwoInt :: Int
channelTwoInt = 0

intensityString :: [Char]
intensityString = replicate 9 ' '

kpdIntensityFloat :: Float
kpdIntensityFloat = 0.0 --global state Haskell no-no

analogRead :: Int -> Int
analogRead pin = -- implementation here

setDmxChannel :: Int -> Int -> [Int] -> [Int]
setDmxChannel ch val dmxVal =
  let (before, after) = splitAt (ch-1) dmxVal
  in before ++ [val] ++ tail after

kpdState :: IORef KeypadState
kpdState = unsafePerformIO (newIORef MODE_SELECT)

keypadLogic :: Bool -> Char -> IO ()
keypadLogic isAnInteger kpdInput = do
  state <- readIORef kpdState
  case state of
    -- MODE_SELECT
    MODE_SELECT ->
      if not isAnInteger && pgmModeSelectionInt > 0
      then case Map.lookup kpdInput kpdModeTable of
             Just (mode, displayMsg) -> do
               smpleDisplay displayMsg True True
               controlMode <- pure mode
               kpdState <- pure NO_CMD
               modeChosen <- pure True
             Nothing -> do
               smpleDisplay "Number not allowed" True True
               kpdState <- pure MODE_SELECT
      else case Map.lookup kpdInput kpdSelectTable of
             Just sel -> do
               pgmModeSelectionInt <- pure sel
               smpleDisplay (show sel) True True
               kpdState <- pure MODE_SELECT
             Nothing -> do
               smpleDisplay "Number not allowed" True True
               kpdState <- pure MODE_SELECT
    -- NO_CMD
    NO_CMD ->
      if not isAnInteger
      then kpdState <- pure NO_CMD
      else if kpdInput == '0'
           then kpdState <- pure NO_CMD
           else do
             chOneKpdChar <- pure (chOneKpdChar ++ [kpdInput])
             smpleDisplay chOneKpdChar True True
             intCount <- pure (intCount + 1)
             kpdState <- pure DMXCH_ONE
    -- DMXCH_ONE
    DMXCH_ONE ->
      if not isAnInteger
      then pure ()
      else if intCount == 2
           then do
             chOneKpdChar <- pure (chOneKpdChar ++ [kpdInput])
             let channelOneInt = min 512 (read chOneKpdChar)
             selectionType <- pure SINGLECHANNEL
             let subIntensity = fromIntegral (analogRead 9) / 65536.0 * 65025.0
                 subIntensity' = if subIntensity <= 0.01 then 0.0 else subIntensity
                 dmxVal' = if null dmxVal then [0,0] else dmxVal
                 dmxVal'' = setDmxChannel chOne channelOneInt (setDmxChannel chTwo (floor subIntensity') dmxVal')
             sendDMX512 dmxVal''
             kpdState <- pure NO_CMD
           else do
             chOneKpdChar <- pure (chOneKpdChar ++ [kpdInput])
             smpleDisplay chOneKpdChar True True
             intCount <- pure (intCount
