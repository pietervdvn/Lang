module Prompt (prompt) where

{--

This module implements an interactive prompt

--}

import StdDef
import StateT
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.Tuple
import Control.Arrow


prompt	:: String -> IO String
prompt question	=
		do	answer 	<- runstateT (clearPrompt question >> ask' question) ("", 0) |> snd |> fst
			return answer



ask'	:: String -> StateT (String, Int) IO ()
ask' question
	= do	clearPrompt question
		done	<- askChar
		unless done $ ask' question

clearPrompt	:: String -> StateT (String, Int) IO ()
clearPrompt question
	= do	let spaces	= 10
		(answer, index)	<- get
		lift$do	putStr $ replicate spaces ' '
			putStr "\r"
			putStr $ question ++ answer ++ replicate spaces ' '
			putStr (replicate (spaces + length answer - index) "\ESC[D" & join)


askChar	:: StateT (String, Int) IO Bool
askChar
	= do	c	<- lift getChar
		if c == '\n' then return True else do
		if c == '\EOT' then put (":exit",5) >> return True else do
		(str, index)	<- get
		case c of
			'\DEL'	-> put (take (index-1) str ++ drop index str, max 0 index-1)
			'\NAK'	-> put ("",0)
			'\ESC'	-> lift (readEscSeq "") >>= handleEscSeq
			_	-> put (take index str ++ [c] ++ drop index str, index + 1)
		return False

data EscSeq	= UP | DOWN | LEFT | RIGHT | DEL | INSERT | END | HOME
	deriving (Show, Eq)


handleEscSeq	:: EscSeq -> StateT (String, Int) IO ()
handleEscSeq UP	= return () -- lift $ putEscSeq DOWN
handleEscSeq DOWN	= return ()-- lift $ putEscSeq UP
handleEscSeq LEFT	= modify (second $ \i -> max 0 (i - 1))
handleEscSeq RIGHT	= modify $ \(str, i) -> (str, min (length str) (i+1))
handleEscSeq DEL	= modify $ \(str, i) -> (take i str ++ drop (i+1) str, i)
handleEscSeq HOME	= modify (second $ \i -> 0)
handleEscSeq END	= modify $ \(str, i) -> (str, length str)


putEscSeq	:: EscSeq -> IO ()
putEscSeq escSq	= do	let errMsg	= "Escape sequence "++show escSq ++ " has no representation and can't be outputted"
			let repr	= escSequences |> swap & lookup escSq & fromMaybe (error errMsg)
			putStr $ "\ESC" ++ repr

maxEscSeqLength	= escSequences |> fst |> length & maximum
escSequences	= [("[A", UP),("[B", DOWN),("[D", LEFT),("[C", RIGHT),("[3~", DEL),("[2~", INSERT),("[F", END),("[H", HOME)]

readEscSeq	:: String -> IO EscSeq
readEscSeq read	=  if length read > maxEscSeqLength then error $ "Unknown escape sequence: "++read else do
			case lookup read escSequences of
				Just seq	-> return seq
				Nothing		-> getChar >>= (\c -> readEscSeq $ read ++ [c])
