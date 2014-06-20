module StdDef where

type Name 	= String
type FileName	= String
type LineNumber = Int
type CharNumber	= Int
type Pos	= Int
type Position	= (LineNumber, CharNumber)

todo	= error "TODO"
todos	= error . (++) "TODO: "

const2		:: a -> b -> c -> a
const2 a _ _ 	=  a

-- non crashing version of init
init'		:: [a] -> [a]
init' []	=  []
init' ls	=  init ls

-- non crashing version of tail
tail'		:: [a] -> [a]
tail' []	=  []
tail' ls	=  tail ls
