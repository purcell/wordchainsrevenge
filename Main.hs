module Main where

import           Data.Text          (pack)
import           System.Environment (getArgs)
import           WordChains

main = do
  [word1, word2] <- getArgs
  dict <- standardDict
  print $ take 1 $ chains dict (pack word1) (pack word2)
