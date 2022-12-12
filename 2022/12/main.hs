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

data FieldType = Start | End | Normal deriving (Eq, Show)
type Cell = (Char, FieldType, Int)

type ParsedLine = [Cell]

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

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

maxDist :: Int
maxDist = 9999999

getCell :: Char -> Cell
getCell 'S' = ('a', Start, maxDist)
getCell 'E' = ('z', End, maxDist)
getCell c = (c, Normal, maxDist)

convert :: String -> ParsedLine
convert = map getCell

type CellMatrix = Array (Int, Int) Cell

getInputAsArray :: [[Cell]] -> CellMatrix
getInputAsArray cells@(c:_)= listArray ((0,0), (length cells - 1, length c - 1)) (concat cells)

startIndices :: FieldType -> CellMatrix -> (Int, Int)
startIndices ft = head . map fst . filter (\(_,(_, t, _)) -> t == ft) . assocs

neighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours (x, y) (maxX, maxY) = filter (\(a, b) -> a >= 0 && b >= 0 && a <= maxX && b <= maxY) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

getNextMoves :: CellMatrix -> (Int, Int) -> Int -> [(Int, Int)]
getNextMoves mx ix cost = filter (\nx -> isValidNeighbour hix (mx ! nx) && cost + 1 < cix) ns
    where
        ns = neighbours ix (snd $ bounds mx)
        (hix, _, cix) = mx ! ix

isValidNeighbour :: Char -> Cell -> Bool
isValidNeighbour hix (h, _, _) = ord h <= ord hix + 1

bfs :: CellMatrix -> (Int, Int) -> Int -> [(Int, Int)] -> [Int] -> Int
bfs mx ix cost pix costs
    | t == End = cost
    | currentCost < cost = bfs mx nextIx nextCost nextPix nextCosts
    | otherwise = bfs nextMx nextIx nextCost nextPix nextCosts
    where
        newPix = getNextMoves mx ix cost
        (nextCost:nextCosts) = costs ++ replicate (length newPix) (cost + 1)
        (nextIx:nextPix) = pix ++ newPix
        (h, t, currentCost) = mx ! ix
        nextMx = mx // [(ix, (h, t, cost))]


convert2 :: String -> ParsedLine2
convert2 = convert

getNextMovesReverse :: CellMatrix -> (Int, Int) -> Int -> [(Int, Int)]
getNextMovesReverse mx ix cost = filter (\nx -> isValidNeighbourReverse hix (mx ! nx) && cost + 1 < cix) ns
    where
        ns = neighbours ix (snd $ bounds mx)
        (hix, _, cix) = mx ! ix

isValidNeighbourReverse :: Char -> Cell -> Bool
isValidNeighbourReverse hix (h, _, _) = ord h >= ord hix - 1

reverseBfs :: CellMatrix -> (Int, Int) -> Int -> [(Int, Int)] -> [Int] -> Int
reverseBfs mx ix cost pix costs
    | h == 'a' = cost
    | currentCost < cost = reverseBfs mx nextIx nextCost nextPix nextCosts
    | otherwise = reverseBfs nextMx nextIx nextCost nextPix nextCosts
    where
        newPix = getNextMovesReverse mx ix cost
        (nextCost:nextCosts) = costs ++ replicate (length newPix) (cost + 1)
        (nextIx:nextPix) = pix ++ newPix
        (h, t, currentCost) = mx ! ix
        nextMx = mx // [(ix, (h, t, cost))]

solve1 :: [ParsedLine] -> Int
solve1 lines = bfs mx startIx 0 [] []
    where
        mx = getInputAsArray lines
        startIx = startIndices Start mx

solve2 :: [ParsedLine2] -> Int
solve2 lines = reverseBfs mx endIx 0 [] []
    where
        mx = getInputAsArray lines
        endIx = startIndices End mx