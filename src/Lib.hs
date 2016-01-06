module Lib
    ( mkSets
    , mkIntSets
    , mkListSets
    ) where

import qualified Data.IntSet     as IS
import qualified Data.Set        as S

mkSets :: Int -> (S.Set Int, S.Set Int)
mkSets order = (S.fromList set1, S.fromList set2)
  where
    (set1, set2) = mkListSets order

mkIntSets :: Int -> (IS.IntSet, IS.IntSet)
mkIntSets order = (IS.fromList set1, IS.fromList set2)
  where
    (set1, set2) = mkListSets order

mkListSets :: Int -> ([Int], [Int])
mkListSets order = (set1, set2)
  where
    size = 10 ^ order
    set1 = [ 0    .. size - 1     ]
    set2 = [ size .. size * 2 - 1 ]
