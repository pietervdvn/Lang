module StdDef where

type Name 	= String
type FileName	= String
type LineNumber = Int
type CharNumber	= Int
type Pos	= Int
type Position	= (LineNumber, CharNumber)

todo	= error "TODO"
todos	= error . (++) "TODO: "

const2		:: a -> b -> c -> c
const2 _ _ 	= id
