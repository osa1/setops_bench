module Main (main) where

import qualified Data.IntSet    as IS
import           Data.List      (nub, union)
import qualified Data.Set       as S

import           Criterion.Main

import           Lib

main :: IO ()
main = defaultMain
  [ mkSetBench 6, mkIntSetBench 6, mkListSetBench 4,
    mkBuildSetBench 6, mkBuildIntSetBench 6, mkBuildListSetBench 4,
    mkBuildAndUnionSetBench 6, mkBuildAndUnionIntSetBench 6,
    mkListUnionViaSetBench 6, mkListUnionViaIntSetBench 6
  ]

mkListSetBench, mkSetBench, mkIntSetBench,
  mkBuildSetBench, mkBuildIntSetBench, mkBuildListSetBench,
  mkBuildAndUnionSetBench, mkBuildAndUnionIntSetBench,
  mkListUnionViaSetBench, mkListUnionViaIntSetBench :: Int -> Benchmark

mkListSetBench order =
    bgroup "List set" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return (mkListSets i)) $ \ ~(set1, set2) ->
        bench ("Sets with size 10^" ++ show i)  (nf (union set1) set2)

mkSetBench order =
    bgroup "Set" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return (mkSets i)) $ \ ~(set1, set2) ->
        bench ("Sets with size 10^" ++ show i) (nf (S.union set1) set2)

mkIntSetBench order =
    bgroup "IntSet" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return (mkIntSets i)) $ \ ~(set1, set2) ->
        bench ("Sets with size 10^" ++ show i) (nf (IS.union set1) set2)

mkBuildSetBench order =
    bgroup "Set - fromList" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      let lst = [ 0 .. (10^i) - 1 ] :: [Int]
      in env (return lst) $ \lst ->
           bench ("S.fromList for size 10^" ++ show i) (nf S.fromList lst)

mkBuildIntSetBench order =
    bgroup "IntSet - fromList" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return [ 0 .. (10^i) - 1 ]) $ \lst ->
        bench ("IS.fromList for size 10^" ++ show i) (nf IS.fromList lst)

mkBuildListSetBench order =
    bgroup "List set - nub" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return [ 0 :: Int .. (10^i) - 1 ]) $ \lst ->
        bench ("nub for size 10^" ++ show i) $
          nf nub lst

mkBuildAndUnionSetBench order =
    bgroup "Set - fromSet and union" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      bench ("S.fromList for size 10^" ++ show i) $ nf go i

    go i =
      let
        size = 10 ^ i

        lst1, lst2 :: [Int]
        lst1 = [ 0    .. size - 1     ]
        lst2 = [ size .. 2 * size - 1 ]
      in
        S.fromList lst1 `S.union` S.fromList lst2

mkBuildAndUnionIntSetBench order =
    bgroup "IntSet - fromSet and union" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      bench ("IS.fromList for size 10^" ++ show i) $ nf go i

    go i =
      let
        size = 10 ^ i
        lst1 = [ 0    .. size - 1     ]
        lst2 = [ size .. 2 * size - 1 ]
      in
        IS.fromList lst1 `IS.union` IS.fromList lst2

mkListUnionViaSetBench order =
    bgroup "List union via Set" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return (mkListSets i)) $ \ ~(set1, set2) ->
        bench ("Build Set, take union, return list back, 10^" ++ show i) $
          nf (\set2 -> S.toList (S.fromList set1 `S.union` S.fromList set2)) set2

mkListUnionViaIntSetBench order =
    bgroup "List union via IntSet" (map mk_bench [ 0 .. order ])
  where
    mk_bench i =
      env (return (mkListSets i)) $ \ ~(set1, set2) ->
        bench ("Build IntSet, take union, return list back, 10^" ++ show i) $
          nf (\set2 -> IS.toList (IS.fromList set1 `IS.union` IS.fromList set2)) set2
