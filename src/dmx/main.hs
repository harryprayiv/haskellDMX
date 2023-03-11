import DmxUniverse

main :: IO ()
main = do
  universeA <- randomDmxUniverse
  print $ unDmxUniverse universeA
  let universeB = emptyDmxUniverse
  print $ unDmxUniverse universeB
  randomSelection <- randomDmxUniverseBool
  print $ unDmxUniverseBool randomSelection