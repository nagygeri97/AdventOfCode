module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = String

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

convert :: String -> ParsedLine
convert = id

convert2 :: String -> ParsedLine2
convert2 = convert

isAllDifferent :: Ord a => [a] -> Bool
isAllDifferent xs = nub sorted == sorted
 where
    sorted = sort xs

firstStart :: Int -> String -> Int -> Int
firstStart n (x:xs) k
 | isAllDifferent (take n (x:xs)) = k
 | otherwise = firstStart n xs (k + 1)
firstStart _ [] k = k

solve :: Int -> String -> Int
solve n s = firstStart n s n

solve1 :: [ParsedLine] -> Int
solve1 input = solve 4 line
 where
    line = head input

solve2 :: [ParsedLine2] -> Int
solve2 input = solve 14 line
 where
    line = head input