module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Monad.State
import Control.Monad
import Control.Applicative
import qualified Data.Set as Set

data Sensor = S (Int, Int) (Int, Int) Int 

type ParsedLine = Sensor

type ParsedLine2 = ParsedLine

main :: IO ()
main = mainLoop [] []

mainLoop :: [ParsedLine] -> [ParsedLine2] -> IO ()
mainLoop xs ys = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ ys
        else do line <- getLine
                mainLoop (xs ++ [convert line]) (ys ++ [convert2 line])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

splitByAll :: Eq a => [a] -> [a] -> [[a]]
splitByAll delims list
    | null last = [first]
    | otherwise = first : splitByAll delims (tail last)
    where
        (first,last) = break (`elem` delims) list

convert :: String -> ParsedLine
convert line = S (sx, sy) (bx, by) r
    where
        [sx, sy, bx, by] = map read . filter (any isDigit) . splitByAll ",:=" $ line
        r = abs (sx - bx) + abs (sy - by)

sensorCoversOfLine :: Int -> Sensor -> Maybe (Int, Int)
sensorCoversOfLine y (S (sx, sy) _ r)
    | smallR < 0 = Nothing
    | otherwise = Just (sx - smallR, sx + smallR)
    where
        smallR = r - abs (y - sy)

addNewCoverToSortedList :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
addNewCoverToSortedList p [] = [p]
addNewCoverToSortedList p@(a, b) l@(f@(x, y):xs)
    | a <= y && b >= x = addNewCoverToSortedList (min a x, max b y) xs
    | b < x = p:l
    | a > y = f:addNewCoverToSortedList (a, b) xs

allCoversOfLine :: Int -> [Sensor] -> [(Int, Int)]
allCoversOfLine y = foldr addNewCoverToSortedList [] . map (fromJust) . filter (isJust) . map (sensorCoversOfLine y)

lengthOfCover :: [(Int, Int)] -> Int
lengthOfCover [] = 0
lengthOfCover ((x, y):xs) = y - x + 1 + lengthOfCover xs

beacons :: [Sensor] -> [(Int, Int)]
beacons = map (\(S _ b _) -> b)

uniqueBeacons :: [(Int, Int)] -> [(Int, Int)]
uniqueBeacons = Set.elems . Set.fromList

beaconsOnLine :: Int -> [(Int, Int)] -> [(Int, Int)]
beaconsOnLine y = filter ((==y) . snd)

convert2 :: String -> ParsedLine2
convert2 = convert

fullyCoversLine :: Int -> Int -> Int -> [(Int, Int)] -> Bool
fullyCoversLine y from to = any (\(a, b) -> a <= from && b >= to)

findMissingCoords :: Int -> Int -> [Sensor] -> (Int, Int)
findMissingCoords from to s = findMissingCoordsHelper from to from s

findMissingCoordsHelper :: Int -> Int -> Int -> [Sensor] -> (Int, Int)
findMissingCoordsHelper from to n s
    | fullyCoversLine n from to allCovs = findMissingCoordsHelper from to (n + 1) s
    | otherwise = (x, n)
    where
        allCovs = allCoversOfLine n s
        relevants = filter (\(a, b) -> (a >= from && a <= to) || (b >= from && b <= to)) allCovs
        x = getMissingLink from to relevants

getMissingLink :: Int -> Int -> [(Int, Int)] -> Int
getMissingLink from to [(_, x), (_, _)] = x + 1
getMissingLink from to [(a, _)]
    | a > from = from
    | otherwise = to

score :: (Int, Int) -> Int
score (x, y) = 4000000 * x + y

solve1 :: [ParsedLine] -> Int
solve1 inputs = len - lenBsOnLine
    where
        bs = beacons inputs
        bsOnLine = beaconsOnLine y (uniqueBeacons bs)
        lenBsOnLine = length bsOnLine
        allCovsOfLine = allCoversOfLine y inputs
        len = lengthOfCover allCovsOfLine
        y = 2000000

solve2 :: [ParsedLine2] -> Int
solve2 = score . findMissingCoords minCoord maxCoord
    where
        minCoord = 0
        maxCoord = 4000000