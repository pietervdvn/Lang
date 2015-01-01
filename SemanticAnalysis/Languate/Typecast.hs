module Languate.Typecast ( searchPath ) where

{-
-
-}
import Data.Maybe

searchPath :: Eq a => [(a, a)] -> a -> a -> Maybe [a]
searchPath links ort ziel =
        let isValid     = (== ziel) . head
            foundPaths  = until (any isValid <||> null) (>>= expandPath links) [[ort]] in
        fmap reverse . listToMaybe .  filter isValid $ foundPaths

expandPath :: Eq a => [(a, a)] -> [a] -> [[a]]
expandPath links path = 
        let foundLinks      =  getLinks (head path) links
            nonCyclicLinks  =  filter (not . (`elem` path)) foundLinks in
        -- append node to path
        map (:path) nonCyclicLinks

getLinks :: Eq a => a -> [(a, a)] -> [a]
getLinks a = map snd . filter ((==a) . fst)

(<||>) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(<||>) f g v = f v || g v

test = [
       ('A', 'B'),
       ('B', 'C'),
       ('C', 'D'),
       ('D', 'E'),
       ('E', 'F'),
       ('A', 'C'),
       ('C', 'A')
       ]
