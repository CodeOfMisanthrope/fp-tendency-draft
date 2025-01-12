-- | This module provides various list processing functions.
module Iter.Sync
  ( iTake,
    iEnumerate,
    iFilter,
    iFirst,
    iLast,
    iRepeat,
    iMap,
    iCount,
    iSlice
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

-- | Returns the first element of a list wrapped in a 'Maybe'.
-- If the list is empty, returns 'Nothing'.
--
-- Example:
--
-- >>> iFirst [1, 2, 3]
-- Just 1
--
-- >>> iFirst []
-- Nothing
iFirst :: [a] -> Maybe a
iFirst [] = Nothing
iFirst (x : xs) = Just x

-- | Returns the last element of a list wrapped in a 'Maybe'.
-- If the list is empty, returns 'Nothing'.
--
-- Example:
--
-- >>> iLast [1, 2, 3]
-- Just 3
--
-- >>> iLast []
-- Nothing
iLast :: [a] -> Maybe a
iLast [] = Nothing
iLast (x : xs) =
  if null xs
    then Just x
    else iLast xs

-- | Repeats a list a specified number of times.
-- If the count is less than or equal to 0, an empty list is returned.
--
-- Example:
--
-- >>> iRepeat 2 [1, 2]
-- [1, 2, 1, 2]
--
-- >>> iRepeat 0 [1, 2]
-- []
iRepeat :: Int -> [a] -> [a]
iRepeat _ [] = []
iRepeat count list
  | count <= 0 = []
  | otherwise = list ++ iRepeat (count - 1) list

-- | Applies a function to each element of a list, returning a new list.
--
-- Example:
--
-- >>> iMap (*2) [1, 2, 3]
-- [2, 4, 6]
--
-- >>> iMap show [1, 2, 3]
-- ["1", "2", "3"]
iMap :: (a -> b) -> [a] -> [b]
iMap _ [] = []
iMap fn (x : xs) = fn x : iMap fn xs

-- | Counts the number of elements in a list.
--
-- Example:
--
-- >>> iCount [1, 2, 3]
-- 3
--
-- >>> iCount []
-- 0
iCount :: [a] -> Int
iCount [] = 0
iCount (_:xs) = 1 + iCount xs

-- | Returns a sublist from a list, starting from the 'start' index
-- and ending before the 'end' index.
-- If 'start' or 'end' is out of bounds, it handles those cases gracefully.
--
-- Example:
--
-- >>> iSlice 1 4 [0, 1, 2, 3, 4, 5]
-- [1, 2, 3]
--
-- >>> iSlice 0 2 [1, 2, 3]
-- [1, 2]
--
-- >>> iSlice 2 2 [1, 2, 3]
-- []
iSlice :: Int -> Int -> [a] -> [a]
iSlice _ _ [] = []
iSlice start end list = take (end - start) (drop start list)
