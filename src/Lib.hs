module Lib
  ( someFunc,
  )
where

import Iter.Sync (iEnumerate, iTake)

takedList = iTake 3 [1 .. 10]
enumeratedList = iEnumerate 0 [1 .. 10]

someFunc :: IO ()
-- someFunc = putStrLn "someFunc"
--someFunc = putStrLn (show takedList)
someFunc = putStrLn (show enumeratedList)
