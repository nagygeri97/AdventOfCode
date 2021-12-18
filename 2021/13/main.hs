module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = String

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn "Part Two: " 
                solve2 xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = id

data Axis = X | Y deriving (Eq, Show)

parseInput :: [ParsedLine] -> ([(Int,Int)], [(Axis, Int)])
parseInput lines = (parsedPts, parsedFolds)
    where
        [points, folds] = splitBy [] lines
        parsedPts = map (fromList . map read . splitBy ',') points
        parsedFolds = map ((\(x,y) -> (if x == "x" then X else Y, read y)) . fromList . splitBy '=' . (!!2) . words) folds
        fromList [a,b] = (a,b)

solve1 :: [ParsedLine] -> Int
solve1 input = length $ performFold pts (head folds)
    where
        (pts, folds) = parseInput input

performFold :: [(Int, Int)] -> (Axis, Int) -> [(Int, Int)]
performFold pts (axis, line) = nub $ map (\pt -> constructFun (let x = axisFun pt in if x > line then line - (x - line) else x) (otherFun pt)) pts
    where
        axisFun = if axis == X then fst else snd
        otherFun = if axis == X then snd else fst
        constructFun = if axis == X then (,) else flip (,)


solve2 :: [ParsedLine] -> IO ()
solve2 input = showListCoords $ foldl performFold pts folds
    where
        (pts, folds) = parseInput input

showListCoords :: [(Int, Int)] -> IO ()
showListCoords pts = putStrLn . unlines . transpose . foldr (replaceAtMatrix '█') empty $ pts
    where
        sortedPts = sort pts
        maxx = maximum $ map fst pts
        maxy = maximum $ map snd pts
        empty = replicate (maxx + 1) (replicate (maxy + 1) ' ')

replaceAtMatrix :: a -> (Int, Int) -> [[a]] -> [[a]]
replaceAtMatrix _ _ [] = []
replaceAtMatrix e (0,y) (c:cs) = replaceAtList e y c : cs
replaceAtMatrix e (x,y) (c:cs) = c : replaceAtMatrix e (x-1,y) cs

replaceAtList :: a -> Int -> [a] -> [a]
replaceAtList e _ [] = []
replaceAtList e 0 (x:xs) = e : xs
replaceAtList e y (x:xs) = x : replaceAtList e (y-1) xs
