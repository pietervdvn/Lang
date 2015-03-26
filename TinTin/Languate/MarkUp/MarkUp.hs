module Languate.MarkUp.MarkUp where

-- This module implements the base definitions of the markup data structure

import StdDef
import State
import Data.Maybe
import Data.List

-- Represents a snippet of markUpped code
data MarkUp
        = Base String		            -- Embeds a plaintext in markup
        | Parag MarkUp                  -- Paragraph for markup
        | Seq [MarkUp]                  -- Sequence of markup
        | Emph MarkUp		            -- Emphasized markup
        | Imp MarkUp 		            -- Important markup
        | Code MarkUp 		            -- Code section
        | Incorr MarkUp 	            -- Incorrect code
        | Titling MarkUp MarkUp         -- Embedded titeling [title, markup]
        | Link MarkUp URL               -- A link with overlay text [markup, url]
        | Table [MarkUp] [[MarkUp]]     -- A table [header, tablerows]
        | List [MarkUp]                 -- Unordered list
        | OrderedList [MarkUp]          -- Ordered list

type URL = String

rewrite     :: (MarkUp -> Maybe MarkUp) -> MarkUp -> MarkUp
rewrite f mu = fromMaybe (rw f mu) (f mu)


rw          :: (MarkUp -> Maybe MarkUp) -> MarkUp -> MarkUp
rw f (Base str)
            = Base str
rw f (Parag mu)
            = Parag $ rewrite f mu
rw f (Seq mus)
            = Seq $ mus |> rewrite f
rw f (Emph mu)
            = Emph $ rewrite f mu
rw f (Imp mu)
            = Imp $ rewrite f mu
rw f (Code mu)
            = Code $ rewrite f mu
rw f (Incorr mu)
            = Incorr $ rewrite f mu
rw f (Titling title mu)
            = Titling (rewrite f title) $ rewrite f mu
rw f (Link mu url)
            =  Link (rewrite f mu) url
rw f (Table mus muss)
            = Table (mus |> rewrite f) $ muss ||>> rewrite f
rw f (List mus)
            = List (mus |> rewrite f)
rw f (OrderedList mus)
            = OrderedList (mus |> rewrite f)


------ EASY ACCESS FUNCTIONS -------

parag  = Parag . Base
emph   = Emph . Base
imp    = Imp . Base
code   = Code . Base
incorr = Incorr . Base
titling str
       = Titling (Base str)
link str url
       = Link (Base str)
