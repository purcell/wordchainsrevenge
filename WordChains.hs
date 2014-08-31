{-# LANGUAGE OverloadedStrings #-}
module WordChains (chains, readDict, standardDict) where

import           Data.Set     (Set)
import qualified Data.Set     as S
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Data.Tree    (Tree)
import qualified Data.Tree    as Tr

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


type Chain = [Word]

treeFrom :: Dictionary -> Word -> Tree Chain
treeFrom dict s = Tr.unfoldTree expand (S.delete s dict, [s])
  where expand :: (Dictionary, Chain) -> (Chain, [(Dictionary, Chain)])
        expand (d, c) = (c, [(S.delete n d, n : c) | n <- S.toList $ neighbours d $ head c])

chains :: Dictionary -> Word -> Word -> [Chain]
chains dict a b = map reverse $ filter reachesTarget $ concat $ Tr.levels $ treeFrom dict' a
  where dict' = S.filter ((T.length a ==) . T.length) dict
        reachesTarget [] = False
        reachesTarget (x:_) = x == b



readDict :: FilePath -> IO Dictionary
readDict path = return . S.fromList . T.lines =<< TIO.readFile path

standardDict :: IO Dictionary
standardDict = readDict "wordlist.txt"
