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

data IOption a = None | Some a
  deriving (Show, Eq)
