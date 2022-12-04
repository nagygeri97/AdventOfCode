module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = ((Int, Int), (Int, Int))

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
convert xs = ((x,y), (w,z))
 where
 [[x,y], [w,z]] =  map (map read . splitBy '-') $splitBy ',' xs

convert2 :: String -> ParsedLine2
convert2 = convert

contains :: ParsedLine -> Bool
contains ((x,y), (w,z)) = (x <= w && y >= z) || (w <= x && z >= y)

contains2 :: ParsedLine -> Bool
contains2 ((x,y), (w,z)) = (y >= w && z >= y) || (z >= x && y >= z)

solve1 :: [ParsedLine] -> Int
solve1 = length . filter contains

solve2 :: [ParsedLine2] -> Int
solve2 = length . filter contains2