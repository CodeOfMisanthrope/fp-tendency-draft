module Iter.Sync
  ( iTake,
    iEnumerate,
    iFilter,
    iFirst
  )
where

iTake :: Int -> [a] -> [a]
iTake _ [] = []
iTake 0 _ = []
iTake n (x : xs) = x : iTake (n - 1) xs

iEnumerate :: Int -> [a] -> [(a, Int)]
iEnumerate _ [] = []
iEnumerate count (x : xs) = (x, count) : iEnumerate (count + 1) xs

iFilter :: (a -> Bool) -> [a] -> [a]
iFilter _ [] = []
iFilter fn (x:xs) = if fn x
  then x : iFilter fn xs
  else iFilter fn xs

iFirst :: [a] -> a
iFirst (x:xs) = x
