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

solve1 :: [ParsedLine] -> Int
solve1 lines = sum validNumbers
    where
        emptyLine = map (const '.') (head lines)
        linesWithNeighbors = zip3 (emptyLine:init lines) lines (tail lines ++ [emptyLine])
        cellsWithNeighbors = map getNeighborsForLine linesWithNeighbors
        numbersWithNeighbors = concatMap (filter (isDigit . fst . head) . groupBy ((==) `on` isDigit . fst)) cellsWithNeighbors
        validNumbers = map (read . map fst) . filter (any isNotDotOrDigit . concatMap snd) $ numbersWithNeighbors
    
isNotDotOrDigit :: Char -> Bool
isNotDotOrDigit c = c `notElem` ".1234567890"

getNeighborsForLine :: (String, String, String) -> [(Char, String)]
getNeighborsForLine (prev, curr, next) = zipWith fn (tail next ++ ".") . zipWith fn ('.':init next) . zipWith fn (tail prev ++ ".") . zipWith fn ('.':init prev) . zipWith fn (tail curr ++ ".") . zipWith fn ('.':init curr) . map (\(p, c, n) ->  (c, [p, n])) $ zip3 prev curr next
    where fn e (c,l) = (c, e:l)

solve2 :: [ParsedLine2] -> Int
solve2 = undefined
    where
        emptyLine = map (const '.') (head lines)
        linesWithNeighbors = zip3 (emptyLine:init lines) lines (tail lines ++ [emptyLine])
        cellsWithNeighbors = map getNeighborsForLine linesWithNeighbors