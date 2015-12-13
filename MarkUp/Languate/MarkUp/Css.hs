module Languate.MarkUp.Css where

import StdDef

import Data.Map (Map, findWithDefault)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data CSS	= CSS 	{ name	:: Name
			, styleTagConts	:: String	-- added to the beginning of each html file
			, renderCSS	:: String}

instance Show CSS where
	show	= renderCSS

defaultCSSCons	= CSS "defaultCSS" "body {\n\tmin-width: 200px;\n\tmax-width: 1080;\n\tmargin: 0 auto;\n\tpadding: 60px;\n\toverflow:scroll }"

-- relative to the root of the package
whiteCSSLocation	= "CSS/White.css"
blackCSSLocation	= "CSS/Black.css"

loadCSS		:: FilePath -> (String -> CSS) -> IO CSS
loadCSS fp cons
	= do	conts	<- readFile fp
		return $ cons conts
