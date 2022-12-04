module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = (String, String)

type ParsedLine2 = String

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
convert xs = splitAt (length xs `div` 2) xs

convert2 :: String -> ParsedLine2
convert2 = id

score :: Char -> Int
score c
 | 'a' <= c && c <= 'z' = ord c - ord 'a' + 1
 | 'A' <= c && c <= 'Z' = ord c - ord 'A' + 27

solve1 :: [ParsedLine] -> Int
solve1 = sum . map (score . head . (uncurry intersect))

commons :: [ParsedLine2] -> [Char]
commons [] = []
commons (x:y:z:xs) = head (intersect (intersect x y) z) : commons xs

solve2 :: [ParsedLine2] -> Int
solve2 = sum . map score . commons