module Lib
    ( mkSets
    , mkIntSets
    , mkListSets
    , mkStringListSet
    , mkStringListSets
    , mkStringSet
    , mkStringSets
    , mkStringGenericSet
    , mkStringGenericSets
    ) where

import           Data.Bifunctor (bimap)

import qualified Data.IntSet    as IS
import qualified Data.Set       as S
import qualified Data.StringSet as SS

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

mkStringListSet :: Int -> [String]
mkStringListSet order = map show (fst (mkListSets order))

mkStringListSets :: Int -> ([String], [String])
mkStringListSets order = bimap (map show) (map show) (mkListSets order)

mkStringSet :: Int -> SS.Set
mkStringSet order = SS.fromList (mkStringListSet order)

mkStringSets :: Int -> (SS.Set, SS.Set)
mkStringSets order = bimap SS.fromList SS.fromList (mkStringListSets order)

mkStringGenericSet :: Int -> S.Set String
mkStringGenericSet order = S.fromList (mkStringListSet order)

mkStringGenericSets :: Int -> (S.Set String, S.Set String)
mkStringGenericSets order = bimap S.fromList S.fromList (mkStringListSets order)
