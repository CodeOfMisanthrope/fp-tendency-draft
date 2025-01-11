module Iter.Sync
  ( iTake,
    iEnumerate,
  )
where

iTake :: Int -> [a] -> [a]
iTake _ [] = []
iTake 0 _ = []
iTake n (x : xs) = x : iTake (n - 1) xs

iEnumerate :: Int -> [a] -> [(a, Int)]
iEnumerate _ [] = []
iEnumerate count (x : xs) = (x, count) : iEnumerate (count + 1) xs
