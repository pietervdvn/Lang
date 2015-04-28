module Normalizable where

{-
A normalizable thing is a data structure that can be written in multiple ways, but has one canonical notation.
normalize bring this in this canonical form.
-}
class Normalizable a where
	normalize	:: a -> a
