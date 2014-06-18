module ObjectNotation where

{--

This module implements a test of object notation.

The defined dot-operator allows expressions as
[1,2,3].map (+1)


--}
import Prelude hiding ((.))

data Game	= Game {rec :: Record}
	deriving Show

data Record	= Record {point :: Point, name :: Name}
	deriving (Show)


data Point	= Point {x :: Int, y :: Int}
	deriving (Show)



infixr 9 .
(.)		:: a -> (a -> b) -> b
(.) a f		=  f a

setX	:: Int -> Point -> Point
setX x (Point _ y)
		= Point x y

setRecord	:: Game -> Record -> Game
setRecord _ r	=  Game r

setPoint	:: Record -> Point -> Record
setPoint (Record _ n) p
		= Record p n

p	= Point 4 2
r	= Record p "Aqmsldkf"
g	= Game r

modPoint		:: (Point -> Point) -> Record -> Record
modPoint f (Record p b)	=  Record (f p) b

modRec		:: (Record -> Record) -> Game -> Game
modRec f (Game r)
		= Game $ f r
