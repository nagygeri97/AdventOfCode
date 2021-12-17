module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = ((Int, Int), (Int, Int))

type ArrayInt a = Array Int a
type Arr = ArrayInt (ArrayInt Int)

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

convert :: String -> ParsedLine
convert s = ((x1,y1), (x2,y2))
    where
        [[x1,y1],[x2,y2]] = (map . map) read . map words $ [first, second]
        [first,_,second] = (map . map) (\x -> if x == ',' then ' ' else x) . words $ s

emptyBoard :: [ParsedLine] -> Arr
emptyBoard input = array (0,size) [(n,array (0,size) [(i,0) | i<-[0..size]]) | n<-[0..size]]
    where 
        size = maximum $ concat $ map (\((x,y),(z,w)) -> [x,y,z,w]) input 

applyLineOnBoard :: ParsedLine -> Arr -> Arr
applyLineOnBoard ((x1,y1), (x2,y2)) board
 | x1 == x2 = board // [(x1, board ! x1 // [(y, (board ! x1 ! y) + 1) | y<-[min y1 y2..max y1 y2]])]
 | y1 == y2 = board // [(x, board ! x // [(y1, (board ! x ! y1) + 1)]) | x<-[min x1 x2..max x1 x2]]
 | otherwise = board

solve1 :: [ParsedLine] -> Int
solve1 input = length . filter (>1) . concat . map elems . elems . foldr applyLineOnBoard (emptyBoard input) $ input


applyLineOnBoard2 :: ParsedLine -> Arr -> Arr
applyLineOnBoard2 coords@((x1,y1), (x2,y2)) board
 | x1 == x2 = board // [(x1, board ! x1 // [(y, (board ! x1 ! y) + 1) | y<-[min y1 y2..max y1 y2]])]
 | y1 == y2 = board // [(x, board ! x // [(y1, (board ! x ! y1) + 1)]) | x<-[min x1 x2..max x1 x2]]
 | otherwise = board // [(x, board ! x // [(y, (board ! x ! y) + 1)]) | (x,y)<-coordsBetween coords]

coordsBetween :: ParsedLine -> [(Int, Int)]
coordsBetween ((x1,y1), (x2,y2))
 | x1 <= x2 && y1 <= y2 = zip [x1..x2] [y1..y2]
 | x1 <= x2 && y1 > y2 = zip [x1..x2] [y1,(y1-1)..y2]
 | x1 > x2 && y1 <= y2 = zip [x1,(x1-1)..x2] [y1..y2]
 | x1 > x2 && y1 > y2 = zip [x1,(x1-1)..x2] [y1,(y1-1)..y2]

solve2 :: [ParsedLine] -> Int
solve2 input = length . filter (>1) . concat . map elems . elems . foldr applyLineOnBoard2 (emptyBoard input) $ input