{-# LANGUAGE OverloadedStrings #-}
module WordChains (chains, readDict, standardDict) where

import           Data.Function (on)
import           Data.List     (groupBy, sortBy)
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as S
import           Data.Text     (Text)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Data.Tree     (Tree)
import qualified Data.Tree     as Tr

type Word = Text
type Dictionary = Set Word

-- | A pair of (init, tail), e.g. ("ei", "ht"), which is "eight" with a letter omitted.
-- Words which differ by only one character will have one identical mask.
type Mask = (Text, Text)

-- | Given "Foo", produce the masks [("", "oo"), ("f", "o"), ("fo", "")]
masks :: Word -> [Mask]
masks = makeMasks . T.toLower
    where makeMasks w = zip (T.inits w) (drop 1 . T.tails $ w)

-- | Produce a list of words groups which share a mask, and are therefore neighbours
neighbourGroups :: Dictionary -> [[Word]]
neighbourGroups dict = map (map fst) $ groupBy ((==) `on` snd) $ sortBy (compare `on` snd) allWordsAndMasks
  where allWordsAndMasks = concatMap (\w -> [(w, m) | m <- masks w]) $ S.toList dict

neighbourMap :: Dictionary -> Map Word (Set Word)
neighbourMap dict = foldr (M.unionWith S.union) M.empty $ concatMap groupMap $ neighbourGroups dict
  where groupMap ws = [ M.singleton w (S.fromList $ filter (/= w) ws) | w <- ws ]


type Chain = [Word]

type NeighbourLookup = Word -> Set Word
dictNeighbourLookup :: Dictionary -> NeighbourLookup
dictNeighbourLookup dict w = M.findWithDefault S.empty w (neighbourMap dict)

neighbourLookupExcluding :: NeighbourLookup -> Word -> NeighbourLookup
neighbourLookupExcluding wrapped exclude = S.delete exclude . wrapped

treeFrom :: NeighbourLookup -> Word -> Tree Chain
treeFrom nl s = Tr.unfoldTree expand (nl, [s])
  where expand :: (NeighbourLookup, Chain) -> (Chain, [(NeighbourLookup, Chain)])
        expand (nl', c) = (c, [(neighbourLookupExcluding nl' n, n : c) | n <- S.toList next])
          where next = nl' $ head c

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
