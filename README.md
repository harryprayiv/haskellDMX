<b>Pockonsolve - Version 0.2.0</b>

<i>Experimental DMX Microcontroller</i>
This is a Haskell port of an old project I did.  I want to learn how to parse strings so I am using an old data model that I understand fully to really understand the problem domain.  Plus, I need a reason to use Haskell and I can't think of a good project to start.



Here's what chatGPT told me just now: 

In Haskell, we define functions using name :: arg1 -> arg2 -> ... -> retType syntax. Here, the kpdToCommand function takes a Char as an argument and returns an IO () action. The IO () type indicates that this function performs I/O and doesn't return any meaningful value.

In kpdToCommand, we then use a case expression to pattern match on the input key and perform the corresponding action. If the key matches one of the characters listed, we call keypadLogic function with the appropriate arguments, else we simply return ()

Note that the implementation of analogRead and setDmxChannel functions are not included, as they are not provided in the original C code. You will need to define these functions separately in your Haskell code.

The kpdSubIntensity function takes four arguments: chOne (an integer), selType (a SelectionMode value), chTwo (an integer), and intensity (a float). It returns a list of pairs, where each pair represents a channel and its corresponding intensity value. The floatmap function is defined locally to help with the calculations.

The case expression handles the different selection modes. For SingleChannel and And, it returns a list with one or two pairs, respectively. For Through, it computes a list of pairs representing the range of channels between chOne and chTwo. Finally, for None, it returns an empty list.

Note that in Haskell, the function does not have side effects (such as setting DMX channels), but rather returns a list of pairs that can be used to update the channels in some other way.

''
import Data.List (foldl')

analogRead :: Int -> Int
analogRead pin = -- implementation here

setDmxChannel :: Int -> Int -> [Int] -> [Int]
setDmxChannel ch val dmxVal =
  let (before, after) = splitAt (ch-1) dmxVal
  in before ++ [val] ++ (tail after)

kpdfaderSubIntensity :: Int -> SelectionMode -> Int -> Int -> [Int] -> [Int]
kpdfaderSubIntensity chOne selType chTwo sMfader dmxVal =
  case selType of
    SINGLECHANNEL ->
      let scalerVal = (fromIntegral (analogRead 9) / 65536.0) * 65025.0
          scalerVal' = if scalerVal <= 0.01 then 0.0 else scalerVal
          dmxVal' = if dmxVal' < 2 then 0 else dmxVal
          dmxVal'' = setDmxChannel chOne dmxVal'
      in [dmxVal'' !! i | i <- [0..(length dmxVal'' - 1)]]
    AND ->
      let scalerVal = (fromIntegral (analogRead 9) / 65536.0) * 65025.0
          scalerVal' = if scalerVal <= 0.01 then 0.0 else scalerVal
          dmxVal' = if dmxVal' < 2 then 0 else dmxVal
          dmxVal'' = setDmxChannel chOne dmxVal'
          dmxVal''' = setDmxChannel chTwo dmxVal'
      in [dmxVal''' !! i | i <- [0..(length dmxVal''' - 1)]]
    THROUGH ->
      let scalerVal = (fromIntegral (analogRead 9) / 65536.0) * 65025.0
          scalerVal' = if scalerVal <= 0.01 then 0.0 else scalerVal
          dmxVal' = map (\i -> if i < 2 then 0 else i) [round ((fromIntegral (analogRead sMfader) / 65536.0) * 255.0 * scalerVal) | i <- [chOne..chTwo]]
          dmxVal'' = foldl' (\acc (i, v) -> setDmxChannel (i+1) v acc) dmxVal (zip [chOne-1..chTwo-1] dmxVal')
      in [dmxVal'' !! i | i <- [0..(length dmxVal'' - 1)]]
    NONE -> dmxVal

kpdToCommand :: Char -> IO ()
kpdToCommand key = case key of
    '@' -> keypadLogic False key
    'T' -> keypadLogic False key
    '&' -> keypadLogic False key
    '-' -> keypadLogic False key
    'E' -> keypadLogic False key
    'S' -> keypadLogic False key
    '0' -> keypadLogic True key
    '1' -> keypadLogic True key
    '2' -> keypadLogic True key
    '3' -> keypadLogic True key
    '4' -> keypadLogic True key
    '5' -> keypadLogic True key
    '6' -> keypadLogic True key
    '7' -> keypadLogic True key
    '8' -> keypadLogic True key
    '9' -> keypadLogic True key
    _   -> return ()


keypadLogic :: Bool -> Char -> IO ()
keypadLogic isAnInteger kpdInput = do
  case kpdState of
    -- MODE_SELECT
    MODE_SELECT ->
      if (not isAnInteger) && pgmModeSelectionInt > 0 then
        case kpdInput of
          'E' ->
            case pgmModeSelectionInt of
              1 -> do
                smpleDisplay "Fader Mode" True True
                controlMode <- pure FADER_MODE
                kpdState <- pure NO_CMD
                modeChosen <- pure True
              2 -> do
                smpleDisplay "Keypad Mode" True True
                controlMode <- pure KPD_MODE
                kpdState <- pure NO_CMD
                modeChosen <- pure True
              3 -> do
                smpleDisplay "Keypad Fader Mode" True True
                controlMode <- pure KPDFADER_MODE
                kpdState <- pure NO_CMD
                modeChosen <- pure True
              _ -> do
                kpdState <- pure MODE_SELECT
          '1' -> do
            pgmModeSelectionInt <- pure 1
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          '2' -> do
            pgmModeSelectionInt <- pure 2
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          '3' -> do
            pgmModeSelectionInt <- pure 3
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          _ -> do
            smpleDisplay "Number not allowed" True True
            kpdState <- pure MODE_SELECT
      else
        case kpdInput of
          '1' -> do
            pgmModeSelectionInt <- pure 1
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          '2' -> do
            pgmModeSelectionInt <- pure 2
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          '3' -> do
            pgmModeSelectionInt <- pure 3
            smpleDisplay (show pgmModeSelectionInt) True True
            kpdState <- pure MODE_SELECT
          _ -> do
            smpleDisplay "Number not allowed" True True
            kpdState <- pure MODE_SELECT
    -- NO_CMD
    NO_CMD ->
      if not isAnInteger then
        kpdState <- pure NO_CMD
      else
        case kpdInput of
          '0' -> kpdState <- pure NO_CMD
          _ -> do
            chOneKpdChar <- pure (chOneKpdChar ++ [kpdInput])
            smpleDisplay chOneKpdChar True True
            intCount <- pure (intCount + 1)
            kpdState <- pure DMXCH_ONE
    -- DMXCH_ONE
    DMXCH_ONE ->
      if not isAnInteger then
        pure ()
      else if intCount == 2 then
        do
          chOneKpdChar <- pure (chOneKpdChar ++ [kpdInput])
          channelOneInt <- pure (read chOneKpdChar)
          if channelOneInt > 512 then
            channelOneInt <- pure 512
          selectionType <- pure SINGLECHANNEL
          kpdState <- pure
''



