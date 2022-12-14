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

type ParsedLine = [(Int, Int)]

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

groupPairs :: [a] -> [(a, a)]
groupPairs [] = []
groupPairs (x:y:xs) = (x, y):groupPairs xs

convert :: String -> ParsedLine
convert = groupPairs . map read . filter (isDigit . head). groupBy ((==) `on` isDigit)

type Board = Array (Int, Int) Cell
data Cell
    = Air
    | Rock
    | Sand
    deriving (Eq, Show)

getBounds :: [ParsedLine] -> ((Int, Int), (Int, Int))
getBounds lines = ((minimum xs, 0), (maximum xs, maximum ys)) 
    where
        ys = map snd . concat $ lines
        xs = map fst . concat $ lines

initialBoard :: [ParsedLine] -> Board
initialBoard lines = array bounds [(i, Air) | i <- range bounds]
    where
        bounds = (getBounds lines)

buildBoard :: [ParsedLine] -> State Board Board
buildBoard [] = get
buildBoard ([x]:xs) = buildBoard xs
buildBoard (((x1, y1):c@(x2, y2):cs):xs) = do
    if x1 == x2
        then modify (addLineY x1 (min y1 y2) (max y1 y2))
        else modify (addLineX y1 (min x1 x2) (max x1 x2))
    buildBoard ((c:cs):xs)

addLineX :: Int -> Int -> Int -> Board -> Board
addLineX y x1 x2 board = board // [((x,y), Rock) | x <- [x1..x2]]

addLineY :: Int -> Int -> Int -> Board -> Board
addLineY x y1 y2 board = board // [((x,y), Rock) | y <- [y1..y2]]

addAllSand :: State Board Board
addAllSand = do
    settled <- propagateSand initialSandPos
    if settled
        then addAllSand
        else get

initialSandPos :: (Int, Int)
initialSandPos = (500, 0)

propagateSand :: (Int, Int) -> State Board Bool
propagateSand (x, y) = do
    board <- get
    let ((minX, minY), (maxX, maxY)) = bounds board
    if y > maxY || x < minX || x > maxX
        then return False
        else do
            let cellBelow = getWithDefault board (x, y + 1) Air
            let cellBelowLeft = getWithDefault board (x - 1, y + 1) Air
            let cellBelowRight = getWithDefault board (x + 1, y + 1) Air
            if cellBelow == Air
                then do 
                    propagateSand (x, y + 1)
                else if cellBelowLeft == Air
                    then do
                        propagateSand (x - 1, y + 1)
                    else if cellBelowRight == Air
                        then do
                            propagateSand (x + 1, y + 1)
                        else do 
                            modify (// [((x,y), Sand)])
                            return True

getWithDefault :: Ix i => Array i e -> i -> e -> e
getWithDefault arr ix e
    | inRange (bounds arr) ix = arr ! ix
    | otherwise = e

buildAndSimulate :: [ParsedLine] -> State Board Int
buildAndSimulate lines = do
    buildBoard lines
    addAllSand
    gets countSand

countSand :: Board -> Int
countSand = length . filter (==Sand) . elems

convert2 :: String -> ParsedLine2
convert2 = convert

buildAndSimulate2 :: [ParsedLine] -> State Board Int
buildAndSimulate2 lines = do
    buildBoard lines
    addAllSand2
    gets countSand

addAllSand2 :: State Board Board
addAllSand2 = do
    settled <- propagateSand initialSandPos
    initialCell <- gets (!initialSandPos)
    if settled && initialCell /= Sand
        then addAllSand2
        else get

solve1 :: [ParsedLine] -> Int
solve1 lines = evalState (buildAndSimulate lines) (initialBoard lines)

solve2 :: [ParsedLine2] -> Int
solve2 lines = evalState (buildAndSimulate2 newLines) (initialBoard newLines)
    where
        ((_, _),(_, maxY)) = getBounds lines
        newLines = [(500 - maxY - 2, maxY + 2), (500 + maxY + 2, maxY + 2)]:lines