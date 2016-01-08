module Main (main) where

--------------------------------------------------------------------------------

import           Control.Monad
import qualified Data.IntSet       as IS
import           Data.List         (intersect, nub, union, (\\))
import qualified Data.Map          as M
import qualified Data.Set          as S

import           Criterion.Main

import           Lib

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain [ unionBench, intersectionBench, lookupBench ]

--------------------------------------------------------------------------------
-- Benchmarks

unionBench :: Benchmark
unionBench =
    bgroup "union" [ union_set, union_intset, union_list,
                     union_list_via_set, union_list_via_intset ]
  where
    union_set =
      bgroup "Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (S.union set1) set2)

    union_intset =
      bgroup "IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkIntSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (IS.union set1) set2)

    union_list =
      bgroup "List" $ flip map [ 0 .. 4 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (union set1) set2)

    union_list_via_set =
      bgroup "List_via_Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (S.union (S.fromList set1)) (S.fromList set2))

    union_list_via_intset =
      bgroup "List_via_IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (IS.union (IS.fromList set1)) (IS.fromList set2))

intersectionBench :: Benchmark
intersectionBench =
    bgroup "intersection" [ inter_set, inter_intset, inter_list,
                            inter_list_via_set, inter_list_via_intset ]
  where
    inter_set =
      bgroup "Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (S.intersection set1) set2)

    inter_intset =
      bgroup "IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkIntSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (IS.intersection set1) set2)

    inter_list =
      bgroup "List" $ flip map [ 0 .. 4 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (intersect set1) set2)

    inter_list_via_set =
      bgroup "List_via_Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (S.intersection (S.fromList set1)) (S.fromList set2))

    inter_list_via_intset =
      bgroup "List_via_IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (mkListSets i)) $ \ ~(set1, set2) ->
          bench (show i) (nf (IS.intersection (IS.fromList set1)) (IS.fromList set2))

lookupBench :: Benchmark
lookupBench =
    bgroup "insertion" [ lookup_set, lookup_intset, lookup_list,
                         lookup_list_via_set, lookup_list_via_intset ]
  where
    lookup_set =
      bgroup "Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (let (set1, _) = mkSets i in (set1, S.size set1))) $ \ ~(set, size) ->
          bench (show i) (nf (S.member (size `div` 2)) set)

    lookup_intset =
      bgroup "IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (let (set1, _) = mkIntSets i in (set1, IS.size set1))) $ \ ~(set, size) ->
          bench (show i) (nf (IS.member (size `div` 2)) set)

    lookup_list =
      bgroup "List" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (let (set1, _) = mkListSets i in (set1, length set1))) $ \ ~(set, size) ->
          bench (show i) (nf (elem (size `div` 2)) set)

    lookup_list_via_set =
      bgroup "List_via_Set" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (let (set1, _) = mkListSets i in (set1, length set1))) $ \ ~(set, size) ->
          bench (show i) (nf (S.member (size `div` 2)) (S.fromList set))

    lookup_list_via_intset =
      bgroup "List_via_IntSet" $ flip map [ 0 .. 6 ] $ \i ->
        env (return (let (set1, _) = mkListSets i in (set1, length set1))) $ \ ~(set, size) ->
          bench (show i) (nf (IS.member (size `div` 2)) (IS.fromList set))
