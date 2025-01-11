module Lib
  ( someFunc,
  )
where

import Iter.Sync (iEnumerate, iTake, iFilter)

takedList = iTake 3 [1 .. 10]
enumeratedList = iEnumerate 1 [1 .. 10]

filteredList = iFilter (\x -> x > 2) [1..10]

someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
--someFunc = putStrLn (show takedList)
--someFunc = putStrLn (show enumeratedList)
someFunc = putStrLn (show filteredList)
