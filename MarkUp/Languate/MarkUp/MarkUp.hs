module Languate.MarkUp.MarkUp where

-- This module implements the base definitions of the markup data structure

import StdDef
import State
import Data.Maybe
import Data.List

import Normalizable

import Control.Arrow

-- Represents a snippet of markUpped code
data MarkUp
        = Base String		        -- Embeds a plaintext in markup
        | Parag MarkUp                  -- Paragraph for markup
        | Seq [MarkUp]                  -- Sequence of markup
        | Emph MarkUp		        -- Emphasized markup
        | Imp MarkUp 		        -- Important markup
	| NonImp MarkUp			-- Not important stuff, e.g. "This page is automatically generated"
        | Code MarkUp 		        -- Code section
        | Incorr MarkUp 	        -- Incorrect code
        | Titling MarkUp MarkUp         -- Embedded titeling [title, markup]
        | Link MarkUp URL               -- A link with overlay text [markup, url]
	| InLink MarkUp Name		-- A link to a document within the cluster
        | Table [MarkUp] [[MarkUp]]     -- A table [header, tablerows]
        | List [MarkUp]                 -- Unordered list
        | OrderedList [MarkUp]          -- Ordered list
	| Embed Name			-- Embeds the document from the cluster into this markup
	deriving (Show)

type URL = String

data MarkUpStructure
	= Str (String -> MarkUp) String
	| One (MarkUp -> MarkUp) MarkUp
	| Two (MarkUp -> MarkUp -> MarkUp) MarkUp MarkUp
	| Multi ([MarkUp] -> MarkUp) [MarkUp]
	| ExtraString (MarkUp -> String -> MarkUp) MarkUp String
	| TableStructure [MarkUp] [[MarkUp]]
-- A general function which extract the markdown into it's constructor + args, within the structure
unpack	:: MarkUp -> MarkUpStructure
unpack (Base str)
	= Str Base str
unpack (Parag mu)
	= One Parag mu
unpack (Seq mus)
            = Multi Seq mus
unpack (Emph mu)
            = One Emph mu
unpack (Imp mu)
            = One Imp mu
unpack (NonImp mu)
	    = One NonImp mu
unpack (Code mu)
            = One Code mu
unpack (Incorr mu)
            = One Incorr mu
unpack (Titling title mu)
            = Two Titling title mu
unpack (Link mu url)
            =  ExtraString Link mu url
unpack (Table mus muss)
            = TableStructure mus muss
unpack (List mus)
            = Multi List mus
unpack (OrderedList mus)
            = Multi OrderedList mus
unpack (InLink mu url)
	    = ExtraString InLink mu url
unpack (Embed url)
	    = Str Embed url

-- rebuild the markup from its structure
repack	:: (MarkUp -> MarkUp) -> MarkUpStructure -> MarkUp
repack f (Str cons str)	= cons str
repack f (One cons mu)	= cons $ f mu
repack f (Two cons mu0 mu1)
			= cons (f mu0) (f mu1)
repack f (Multi cons mus)
			= mus |> f & cons
repack f (ExtraString cons mu str)
			= cons (f mu) str
repack f (TableStructure mus muss)
			= Table (mus |> f) (muss ||>> f)

-- Traverses the MarkUp. When on a node a 'a' is found, this a is returned. Children of this node  are not traversed
search	:: (MarkUp -> Maybe a) -> MarkUp -> [a]
search f mu	= fromMaybe (unpack mu & flatten >>= search f) (f mu |> (:[]))

flatten	:: MarkUpStructure -> [MarkUp]
flatten (Str _ _)	= []
flatten (One _ mu)	= [mu]
flatten (Two _ mu0 mu1)	= [mu0, mu1]
flatten (Multi _ mus)	= mus
flatten (ExtraString _ mu _)
			= [mu]
flatten (TableStructure mus muss)
			= mus ++ concat muss


rewrite     :: (MarkUp -> Maybe MarkUp) -> MarkUp -> MarkUp
rewrite f mu = fromMaybe (repack (rewrite f) $ unpack mu) (f mu)

deepRewrite	:: (MarkUp ->  Maybe MarkUp) -> MarkUp -> MarkUp
deepRewrite f mu
	= fromMaybe mu (f mu) & unpack & repack (deepRewrite f)

instance Normalizable MarkUp where
	normalize	= _nm


instance Normalizable MarkUpStructure where
	normalize 	= _ns


_ns		:: MarkUpStructure -> MarkUpStructure
_ns (Multi cons mus)
		= Multi cons (mus |> normalize & filter (not . isEmpty))
_ns (TableStructure mus muss)
		= TableStructure (mus |> normalize & filter (not . isEmpty))
			(muss	||>> normalize
				||>> (id &&& isEmpty)	-- [[(content, isEmpty)]]
				|> unzip		-- [([content], [isEmpty])]
				||>> and		-- [([content], isEmpty)]
				& filter (not . snd)
				|> fst)
_ns struc	= struc


_nm		:: MarkUp -> MarkUp
_nm (Seq mus)	= case mus & filter (not . isEmpty) of
			[mu]	-> mu
			mus	-> Seq mus
_nm mu		=  repack id $ normalize $ unpack mu

isEmpty		:: MarkUp -> Bool
isEmpty mu	= unpack mu & isEmpty'

isEmpty'	:: MarkUpStructure -> Bool
isEmpty' (Str _ str)
		= null $ strip str
isEmpty' (One _ mu)
		= isEmpty mu
isEmpty' (Two _ mu0 mu1)
		= isEmpty mu0 && isEmpty mu1
isEmpty' (Multi _ mus)
		= null mus || (mus |> isEmpty & and)
isEmpty' (ExtraString _ mu str)
		= isEmpty mu && null (strip str)
isEmpty' (TableStructure mus muss)
		= null mus || null muss ||
			((mus |> isEmpty & and) && (muss ||>> isEmpty |> and & and))

toText	:: MarkUp -> String
toText 	= concat . search _toText

_toText	:: MarkUp -> Maybe String
_toText (Base str)	= Just str
_toText _	= Nothing

------ EASY ACCESS FUNCTIONS -------

parag  = Parag . Base
emph   = Emph . Base
imp    = Imp . Base
code   = Code . Base
incorr = Incorr . Base
titling str
       = Titling (Base str)
titling' str mu
	= if not $ isEmpty mu then titling str mu else Seq []
link' str
       = Link (Base str)
link str
	= link' str str
inlink str
	= inlink' str str
inLink	= inlink
inlink' str
	= InLink (Base str)
inLink'	= inlink'
notImportant
	= NonImp . Base
table header
	= Table (header |> Base)
table' header mus
	| null mus	= Seq []
	| otherwise	= table header mus
commas' mus
	= Seq $ intersperse (Base ", ") mus
parags mus
	= mus |> Parag & Seq
unwords'= Seq


-- removes obsolete columns from the table
cleanTable	:: MarkUp -> MarkUp
cleanTable (Table mus muss)
	= case (mus:muss) & transpose & filter (not . emptyRow . drop 1) & transpose of
		[]	-> Seq []
		(mus':muss')
			-> Table mus' muss'
cleanTable mu	= mu

emptyRow	:: [MarkUp] -> Bool
emptyRow row	= row |> isEmpty & and
