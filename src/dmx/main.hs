import DmxUniverse

main :: IO ()
main = do
  universeA <- randomDmxUniverse
  print $ unDmxUniverse universeA
  let universeB = emptyDmxUniverse
  print $ unDmxUniverse universeB
  randomSelection <- randomDmxUniverseBool
  print $ unDmxUniverseBool randomSelection

-- ``` 
-- modify main section to print out the values of universeA then print out the values of universeB then print out the values of randomSelection for debugging purposes
