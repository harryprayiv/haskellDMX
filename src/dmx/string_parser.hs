import Text.Regex.Posix
import Data.Word
import DmxUniverse

type Command = (Int, Int, Word8, String) -- start index, end index, value, value type

parseCommand :: String -> Maybe Command
parseCommand str =
  case str =~ "^(\\d+)\\s+through\\s+(\\d+)\\s+at\\s+(\\d+)%$" of
    [[_, s, e, v]] -> Just (read s, read e, round (read v / 100 * 255), "percentage")
    [[_, i, v]] -> Just (read i, read i, round (read v / 100 * 255), "percentage")
    _ -> Nothing

createCommandOutput :: Command -> DmxUniverse
createCommandOutput (start, end, value, "percentage") =
  let universe = emptyDmxUniverse in
  setDmxChVals universe (DmxUniverse $ replicate start False ++ replicate (end-start+1) True ++ replicate (512-end) False) value
createCommandOutput (start, end, value, "absolute") =
  let universe = emptyDmxUniverse in
  setDmxChVals universe (DmxUniverseBool $ replicate start False ++ replicate (end-start+1) True ++ replicate (512-end) False value)

-- Example usage
main :: IO ()
main = do
  let commandStr = "1 through 512 at 100%"
      maybeCommand = parseCommand commandStr
      maybeOutput = maybeCommand >>= return . createCommandOutput
  case maybeOutput of
    Just output -> putStrLn $ show (unDmxUniverse output)
    Nothing -> putStrLn "Invalid command"
