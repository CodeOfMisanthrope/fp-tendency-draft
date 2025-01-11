-- | This module provides various list processing functions.
module Iter.Sync
  ( iTake,
    iEnumerate,
    iFilter,
    iFirst,
    iLast,
    iRepeat,
    iMap,
    iCount
  )
where

-- | Takes the first 'n' elements from a list.
-- If 'n' is 0 or the list is empty, an empty list is returned.
--
-- Example:
--
-- >>> iTake 3 [1, 2, 3, 4, 5]
-- [1, 2, 3]
--
-- >>> iTake 0 [1, 2, 3]
-- []
iTake :: Int -> [a] -> [a]
iTake _ [] = []
iTake 0 _ = []
iTake n (x : xs) = x : iTake (n - 1) xs

-- | Enumerates the elements of a list, returning each element
-- along with its index (starting from the specified count).
--
-- Example:
--
-- >>> iEnumerate 0 ["a", "b", "c"]
-- [("a", 0), ("b", 1), ("c", 2)]
--
-- >>> iEnumerate 1 [10, 20, 30]
-- [(10, 1), (20, 2), (30, 3)]
iEnumerate :: Int -> [a] -> [(a, Int)]
iEnumerate _ [] = []
iEnumerate count (x : xs) = (x, count) : iEnumerate (count + 1) xs

-- | Filters a list based on a predicate function.
-- Only the elements for which the predicate returns 'True' are included in the result.
--
-- Example:
--
-- >>> iFilter even [1, 2, 3, 4, 5]
-- [2, 4]
--
-- >>> iFilter (> 2) [1, 2, 3, 4, 5]
-- [3, 4, 5]
iFilter :: (a -> Bool) -> [a] -> [a]
iFilter _ [] = []
iFilter fn (x : xs) =
  if fn x
    then x : iFilter fn xs
    else iFilter fn xs

-- todo Maybe
iFirst :: [a] -> a
iFirst (x : xs) = x

iLast :: [a] -> a
iLast (x : xs) =
  if length xs == 0
    then x
    else iLast xs

iRepeat :: Int -> [a] -> [a]
iRepeat _ [] = []
iRepeat count list
  | count <= 0 = []
  | otherwise = list ++ iRepeat (count - 1) list

iMap :: (a -> b) -> [a] -> [b]
iMap _ [] = []
iMap fn (x : xs) = fn x : iMap fn xs

iCount :: [a] -> Int
iCount [] = 0
iCount (_:xs) = 1 + iCount xs
