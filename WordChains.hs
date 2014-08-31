{-# LANGUAGE OverloadedStrings #-}
module WordChains (chains, readDict, standardDict) where

import           Control.Arrow   ((&&&))
import qualified Data.Map        as M
import qualified Data.Map.Strict as MS
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Data.Tree       (Tree)
import qualified Data.Tree       as Tr

type Word = Text
type Dictionary = Set Word

neighbours :: Dictionary -> Word -> Set Word
neighbours dict w = S.filter (areNeighbours w) dict

areNeighbours :: Word -> Word -> Bool
areNeighbours a b | T.length a /= T.length b = False
areNeighbours a b | a == b = False
areNeighbours a b = isLength1 $ filter (uncurry (/=)) $ T.zip a b
  where isLength1 [_] = True
        isLength1 _   = False

neighbourMap :: Dictionary -> MS.Map Word (Set Word)
neighbourMap dict = M.fromList $ map (id &&& neighbours dict) $ S.toList dict


type Chain = [Word]

type NeighbourLookup = Word -> Set Word
dictNeighbourLookup :: Dictionary -> NeighbourLookup
dictNeighbourLookup dict w = M.findWithDefault S.empty w (neighbourMap dict)

neighbourLookupWithout :: NeighbourLookup -> Word -> NeighbourLookup
neighbourLookupWithout wrapped exclude = S.delete exclude . wrapped

treeFrom :: NeighbourLookup -> Word -> Tree Chain
treeFrom nl s = Tr.unfoldTree expand (nl, [s])
  where expand :: (NeighbourLookup, Chain) -> (Chain, [(NeighbourLookup, Chain)])
        expand (nl', c) = (c, [(neighbourLookupWithout nl' n, n : c) | n <- S.toList $ nl' $ head c])

chains :: Dictionary -> Word -> Word -> [Chain]
chains dict a b = map reverse . filter reachesTarget . concat . Tr.levels $ tree
  where dict' = S.filter ((T.length a ==) . T.length) dict
        tree = treeFrom (dictNeighbourLookup dict') a
        reachesTarget [] = False
        reachesTarget (x:_) = x == b



readDict :: FilePath -> IO Dictionary
readDict path = return . S.fromList . T.lines =<< TIO.readFile path

standardDict :: IO Dictionary
standardDict = readDict "wordlist.txt"
