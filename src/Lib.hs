module Lib
  ( someFunc,
  )
where

--import Iter.Sync (iEnumerate, iTake, iFilter, iFirst, iLast, iRepeat, iMap, iCount, iSlice)
--import Containers.Option (IOption(Some, None))
--import qualified Containers.Option (IOption)
import Containers.Option (IOption(Some,None))

--takedList = iTake 3 [1 .. 10]
--enumeratedList = iEnumerate 1 [1 .. 10]
--filteredList = iFilter (\x -> x > 2) [1..10]
--firstEl = iFirst [1..10]
--lastEl = iLast [1..10]
--repeatedList = iRepeat 3 [1..3]
--mapList = iMap (\x -> x * 3) [1..3]
--countEls = iCount [1..7]
--slicedList = iSlice 2 7 [1..7]
--
--someFunc :: IO ()
---- someFunc = putStrLn "someFunc"
----someFunc = putStrLn (show takedList)
----someFunc = putStrLn (show enumeratedList)
----someFunc = putStrLn (show filteredList)
----someFunc = putStrLn (show firstEl)
----someFunc = putStrLn (show lastEl)
----someFunc = putStrLn (show repeatedList)
----someFunc = putStrLn (show mapList)
--someFunc = putStrLn (show slicedList)

--someFunc :: IO ()
--someFunc = do
--  let val1 = Some 2
--  let val2 = None
--  Containers.Option.unwrap
--  putStrLn $ "Unwrapping Some 42: " ++ show (unwa)
--
--  putStrLn "123"

someFunc :: IO ()
someFunc = do
  let val1 = Some 2
  let val2 = None
  case val1 of
    Some v -> putStrLn $ "val: " ++ show v
    None -> putStrLn "not val"

  putStrLn "123"
