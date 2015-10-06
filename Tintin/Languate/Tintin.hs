module Languate.Tintin (generateDocs) where

import StdDef
import Languate.MarkUp as Mu
import Languate.Package
import Languate.TableOverview

import Languate.Index


generateDocs	:: String -> Package -> TableOverview -> (RenderSettings -> RenderSettings, Cluster)
generateDocs time p to
	= let	indexDoc	= index p to
		cluster	= buildCluster [indexDoc] in
		(renderSettings (title indexDoc) time , add to cluster)

renderSettings	:: String -> String -> RenderSettings -> RenderSettings
renderSettings index time
	= addFooter index time . addHeader



links		:: MarkUp -> Maybe MarkUp
links (InLink mu nm)
		= Just $ InLink mu (nm ++ ".html")
links _		= Nothing

addFooter	:: String -> String -> RenderSettings -> RenderSettings
addFooter index	= addPreprocessor' . footer index

footer index time mu
		= Mu.Seq [back index, mu,
				NonImp $ parags [
				Base $ "This doc was automatically generated on "++show time,
				Base "Do not edit it, as changes will be erased the next generation.",
				back index]]

back index 	= NonImp $ InLink (Base "Back to index") index

addHeader	:: RenderSettings -> RenderSettings
addHeader	= addPreprocessor (\doc -> preprocess (\mu -> titling (title doc) $ Mu.Seq [notImportant $ description doc, contents doc ]) doc)