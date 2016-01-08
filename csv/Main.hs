{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Main where

--------------------------------------------------------------------------------

import           Control.Monad
import           Data.Bifunctor     (second)
import           Data.Char          (isDigit)
import           Data.List          (intercalate, isPrefixOf, partition, sortOn,
                                     span)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import qualified Data.Set           as S
import           Safe               (readMay)
import           System.Directory   (createDirectory, doesDirectoryExist,
                                     getCurrentDirectory, getDirectoryContents)
import           System.Environment (getArgs)
import           System.FilePath    ((<.>), (</>))

import           Debug.Trace

--------------------------------------------------------------------------------

main :: IO ()
main = do
    csvFile <- head <$> getArgs
    putStrLn $ "Processing csv file: " ++ csvFile
    csvData <- parseCSV csvFile
    let plotData = splitBenches (splitLines csvData)
    -- print plotData

    -- Create a fresh directory at the current directory
    curDir  <- getCurrentDirectory
    dirPath <- findDir curDir "plot_csvs"
    createDirectory dirPath

    putStrLn $ "Dumping files to " ++ dirPath

    forM_ (zip [1 :: Int ..] plotData) $ \(idx, b :: PlotData) -> do
      -- putStrLn ("[" ++ show idx ++ "] " ++ show b)
      createDirectory (dirPath </> pdBenchName b)
      forM (pdLines b) $ \line -> do
        let fname = dirPath </> pdBenchName b </> lineName line <.> "csv"
        writeFile fname $ unlines $
          map (\(s1, s2) -> s1 ++ "," ++ s2) $ sortOn fst $ values line

      putStrLn "Dumping gpl files..."
      let gplFile = mkGplFile (pdBenchName b) (map lineName (pdLines b))
                              "Size (power of 10)" "Seconds" (0, 6)
      writeFile (dirPath </> pdBenchName b </> pdBenchName b <.> "gpl") gplFile
      writeFile (dirPath </> pdBenchName b </> styleFileName) styleFile
      putStrLn "Done."

findDir :: FilePath -> FilePath -> IO FilePath
findDir root dir = do
    exists <- doesDirectoryExist (root </> dir)
    if exists
      then do
        dirs <- filter (dir `isPrefixOf`) <$> getDirectoryContents root
        let max_num =
              maximum (map (fromMaybe 0 . readMay . reverse . takeWhile isDigit . reverse) dirs)
        return (root </> (dir ++ show (max_num + 1)))
      else return (root </> dir)

-- | Parse a Criterion-generated CSV file. Only parses (Name, Mean) parts.
parseCSV :: FilePath -> IO [(String, String)]
parseCSV file = do
    lines <- map (splitOn ",") . lines <$> readFile file
    case lines of
      (("Name" : "Mean" : _) : rest) ->
        return (map (\l -> (l !! 0, l !! 1)) rest)
      _ -> error "parseCSV: CSV file doesn't seem to be a Criterion-generated CSV."

data PlotData = PlotData
  { pdBenchName :: String
  , pdLines     :: [Line]
  } deriving (Show)

data Line = Line
  { benchName :: String
  , lineName  :: String
  , values    :: [(String, String)]
  } deriving (Show)

-- Split on '/'. First split is benchmark name, second split is line name in
-- plot, third split is value on X axis.
splitLines :: [(String, String)] -> [Line]
splitLines xs = go [] xs
  where
    go plots [] = plots
    go plots ((x, val) : xs) =
      case splitOn "/" x of
        [ bn, ln, x ] -> go (add_to_bench bn plots ln x val) xs
        _ -> trace ("Can't parse " ++ show x ++ ", skipping.") (go plots xs)

    add_to_bench :: String -> [Line] -> String -> String -> String -> [Line]
    add_to_bench bench_name [] line_name x_axis value =
      [ Line bench_name line_name [ (x_axis, value) ] ]

    add_to_bench bench_name (plot : plots) line_name x_axis value
      | bench_name == benchName plot && line_name == lineName plot
      = Line bench_name line_name ((x_axis, value) : values plot) : plots
      | otherwise
      = plot : add_to_bench bench_name plots line_name x_axis value

splitBenches :: [Line] -> [PlotData]
splitBenches [] = []
splitBenches (l@(Line bname _ _) : rest) =
    let (bench_lines, other_lines) = partition ((==) bname . benchName) rest
     in PlotData bname (l : bench_lines) : splitBenches other_lines

mkGplFile :: String -> [String] -> String -> String -> (Int, Int) -> String
mkGplFile bench_name line_names x_label y_label (range_start, range_end) =
  unlines
    [ "#!/usr/bin/env gnuplot"
    , ""
    , "set title \"Length\""
    , ""
    , "set terminal pdfcairo font \"Gill Sans,6\" linewidth 3 rounded dashed size 4in,3in"
    , "set output \"" ++ bench_name ++ ".pdf\""
    , ""
    , "load \"common_styles.gnuplot\""
    , "set key left top noreverse enhanced autotitles nobox"
    , ""
    , "set xrange [" ++ show range_start ++ ":" ++ show range_end ++ "]"
    , "set log y"
    , ""
    , "set xlabel " ++ show x_label
    , "set ylabel " ++ show y_label
    , ""
    , "set datafile separator \",\""
    , "plot \\"
    ] ++ (intercalate ",\\\n" (zipWith' [ 1 :: Int .. ] line_names $ \i line_name ->
      "    \"" ++ line_name ++ ".csv\" using 1:2 w lp ls " ++ show i ++ " title " ++ show line_name))

styleFile :: String
styleFile = unlines
  [ "set pointsize 1.0"
  , "set style line 80 lt rgb \"#222222\""
  , ""
  , "# Line style for grid"
  , "set style line 81 lt 3  # dashed"
  , "set style line 81 lt rgb \"#AAAAAA\"  # grey"
  , ""
  , "set grid back linestyle 81"
  , "set border 3 back linestyle 80 # Remove border on top and right. These"
  , "                               # borders are useless and make it harder"
  , "                               # to see plotted lines near the border."
  , "    # Also, put it in grey; no need for so much emphasis on a border."
  , "set xtics nomirror"
  , "set ytics nomirror"
  , ""
  , "set mytics 10    # Makes logscale look good."
  , "set mxtics 10    # Makes logscale look good."
  , ""
  , "# Line styles: try to pick pleasing colors, rather"
  , "# than strictly primary colors or hard-to-see colors"
  , "# like gnuplot's default yellow.  Make the lines thick"
  , "# so they're easy to see in small plots in papers."
  , "set style line 1  lt rgb \"#A00000\" lw 2 pt 1"
  , "set style line 2  lt rgb \"#00A000\" lw 2 pt 6"
  , "set style line 3  lt rgb \"#5060D0\" lw 2 pt 2"
  , "set style line 4  lt rgb \"#F25900\" lw 2 pt 9"
  , "set style line 5  lt rgb \"#5050A0\" lw 2 pt 4"
  , "set style line 6  lt rgb \"#0050A9\" lw 2 pt 7"
  , "set style line 7  lt rgb \"#445111\" lw 2 pt 6"
  , "set style line 8  lt rgb \"#035009\" lw 2 pt 4"
  , "set style line 9  lt 3 lc rgb \"#406030\" lw 2 pt 8"
  , "set style line 10 lt 4 lc rgb \"#5020F0\" lw 2 pt 5"
  , "set style line 11 lt 1 lc rgb \"#BB50A9\" lw 2 pt 3"
  , "set style line 12 lt 2 lc rgb \"#00BBA9\" lw 2 pt 1"
  ]

styleFileName :: String
styleFileName = "common_styles.gnuplot"

-- | 'zipWith' done right.
zipWith' :: [a] -> [b] -> (a -> b -> c) -> [c]
zipWith' as bs f = zipWith f as bs
