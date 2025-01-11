module Lib
    ( someFunc
    ) where

import Iter.Sync (iTake)

takedList = iTake 3 [1..10]

someFunc :: IO ()
--someFunc = putStrLn "someFunc"
someFunc = putStrLn (show takedList)
