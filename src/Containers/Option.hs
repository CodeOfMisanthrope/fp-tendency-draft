--module Containers.Option
--  ( IOption(Some,None),
--  unwrap,
--  some,
--  none
--  )
--where
--
--data IOption a = None | Some a
--  deriving (Show, Eq)
--
--unwrap :: IOption a -> a
--unwrap (Some x) = x
--unwrap None = error "Attempted to unwrap a None value"
--
--some :: IOption a -> (a -> IOption b) -> IOption b
--some None _ = None
--some (Some x) f = f x
--
--none :: IOption a -> a -> a
--none None alt = alt
--none (Some x) _ = x

module Containers.Option
  ( IOption(Some,None),
  )
where

-- | The 'IOption' type represents an optional value that can either contain a value of type 'a'
-- or indicate the absence of a value. This is analogous to the 'Option' type in Rust.
--
-- The 'IOption' type has two constructors:
--
-- * 'Some a': Represents the presence of a value of type 'a'.
-- * 'None': Represents the absence of a value.
--
-- Example Usage:
--
-- >>> let value1 = Some 42
-- >>> let value2 = None
-- >>> value1
-- Some 42
-- >>> value2
-- None
-- >>> case value1 of
-- ...   Some v -> "Contains: " ++ show v
-- ...   None    -> "No value"
-- "Contains: 42"
--
-- >>> case value2 of
-- ...   Some v -> "Contains: " ++ show v
-- ...   None    -> "No value"
-- "No value"
--
-- The 'IOption' type can also be used in functions that require optional parameters.
--
-- Example Function:
--
-- >>> processOption :: IOption Int -> String
-- >>> processOption (Some 10)
-- "Contains value: 10"
-- >>> processOption None
-- "Does not contain a value"
data IOption a = None | Some a
  deriving (Show, Eq)
