module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ArrayInt a = Array Int a

type ParsedLine = ArrayInt Int
type Arr = ArrayInt ParsedLine

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ listToArray xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ listToArray xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

listToArray :: [a] -> ArrayInt a
listToArray xs = array (0, length xs - 1) (zip [0..] xs)

convert :: String -> ParsedLine
convert = listToArray . map (\x -> read [x])

solve1 :: Arr -> Int
solve1 arr = sum . map ((+1) . (\(x,y) -> arr ! x ! y) . fst) . filter (uncurry $ areNeighboursLarger arr) $ ns
    where
        (_, maxx) = bounds arr
        (_, maxy) = bounds (arr ! 0) 
        is = cross (indices arr) (indices (arr ! 0))
        ns = zip is $ map (neighbours maxx maxy) is

areNeighboursLarger :: Arr -> (Int, Int) -> [(Int, Int)] -> Bool
areNeighboursLarger arr (x,y) = all (\(nx,ny) -> arr ! x ! y < arr ! nx ! ny)

cross :: [a] -> [b] -> [(a,b)]
cross = liftA2 (,) 

neighbours :: Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbours maxx maxy (x,y) = filter (\(x,y) -> x >= 0 && x <= maxx && y >= 0 && y <= maxy) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]

solve2 :: Arr -> Int
solve2 arr = product $ take 3 $ sortBy (flip compare) sizes
    where
        (_, maxx) = bounds arr
        (_, maxy) = bounds (arr ! 0) 
        is = cross (indices arr) (indices (arr ! 0))
        ns = zip is $ map (neighbours maxx maxy) is
        lows = map (fst) . filter (uncurry $ areNeighboursLarger arr) $ ns
        startBasins = map (\idx -> B [idx] [idx]) lows
        allBasins = map (expandBasin arr) startBasins
        sizes = map (length . alls) allBasins

expandBasin :: Arr -> Basin -> Basin
expandBasin arr (B alls (x:xs)) = expandBasin arr (B (openNs ++ alls) (openNs ++ xs))
    where
        (_, maxx) = bounds arr
        (_, maxy) = bounds (arr ! 0) 
        openNs = filter (\(x,y) -> arr ! x ! y < 9) . filter (not . (`elem` alls)) $ neighbours maxx maxy x
expandBasin arr b = b

data Basin = B {alls :: [(Int, Int)], edges :: [(Int, Int)]} deriving (Eq, Show)