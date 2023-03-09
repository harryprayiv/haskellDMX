import Data.List (foldl')

data SelectionMode = SingleChannel | And | Through | None deriving Eq

analogRead :: Int -> Int
analogRead pin = -- implementation here

setDmxChannel :: Int -> Int -> [Int] -> [Int]
setDmxChannel ch val dmxVal =
  let (before, after) = splitAt (ch-1) dmxVal
  in before ++ [val] ++ (tail after)

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


kpdSubIntensity :: Int -> SelectionMode -> Int -> Float -> [(Int, Int)]
kpdSubIntensity chOne selType chTwo intensity =
    case selType of
        SingleChannel -> [(chOne, round $ floatmap intensity 0 0.99999999 0 255)]
        And           -> [(chOne, round $ floatmap intensity 0 0.99999999 0 255), (chTwo, round $ floatmap intensity 0 0.99999999 0 255)]
        Through
            | chOne == chTwo ->
                [(chOne, round $ floatmap intensity 0 0.99999999 0 255)]
            | chOne < chTwo ->
                let range = [chOne..chTwo]
                    vals = map (\i -> (i, round $ floatmap intensity 0 0.99999999 0 255)) range
                in map (\(i, v) -> (i+1, v)) vals
            | chOne > chTwo ->
                let range = [chTwo..chOne]
                    vals = map (\i -> (i, round $ floatmap intensity 0 0.99999999 0 255)) range
                in map (\(i, v) -> (i+1, v)) vals
        None -> []
    where floatmap x in_min in_max out_min out_max = (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min
