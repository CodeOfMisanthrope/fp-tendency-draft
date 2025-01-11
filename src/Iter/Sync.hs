module Iter.Sync
  ( iTake,
  )
where

iTake :: Int -> [a] -> [a]
iTake _ [] = []
iTake 0 _ = []
iTake n (x : xs) = x : iTake (n - 1) xs
