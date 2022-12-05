module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = String

type Stack a = [a]
type Stacks a = Array Int (Stack a)
data Move = Move {
    amount :: Int,
    from :: Int,
    to :: Int
    } deriving (Eq, Show)

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

parseInput :: [ParsedLine] -> (Stacks Char, [Move])
parseInput lines = (parseStacks rawStacks, parseMoves rawMoves)
 where
    (rawStacks, rawMoves) = splitInput lines

splitInput :: [ParsedLine] -> ([String], [String])
splitInput input = (rawStacks, tail rawMoves)
 where 
    (rawStacks, rawMoves) = break null input

parseStacks :: [String] -> Stacks Char
parseStacks lines = listArray (1, length stacks) stacks
 where
    stacks = map (dropWhile (==' ')) . transpose . map everyThird . init $ lines

parseMoves :: [String] -> [Move]
parseMoves = map parseMove

parseMove :: String -> Move
parseMove line = Move amount from to
 where 
    [amount, from, to] = map read . filter (all isDigit) . splitBy ' ' $ line

performMove :: (Stack Char -> [Char] -> Stack Char) -> Move -> Stacks Char -> Stacks Char
performMove moveFun (Move amount from to) stacks = stacks // [(from, fromStackAfter), (to, toStackAfter)] 
 where
    fromStack = stacks ! from
    toStack = stacks ! to
    (elemsToMove, fromStackAfter) = splitAt amount fromStack
    toStackAfter = moveFun toStack elemsToMove

moveWithFlip :: Stack Char -> [Char] -> Stack Char
moveWithFlip = foldl (flip (:))

moveWithoutFlip :: Stack Char -> [Char] -> Stack Char
moveWithoutFlip = foldr (:)

everyThird :: [a] -> [a]
everyThird (_:x:_:xs) = x : everyThird end
 where
    end
     | null xs = []
     | otherwise = tail xs
everyThird [] = []

solve1 :: [ParsedLine] -> String
solve1 input = map head $ elems $ foldl (flip (performMove moveWithFlip)) stacks moves
 where
    (stacks, moves) = parseInput input

solve2 :: [ParsedLine2] -> String
solve2 input = map head $ elems $ foldl (flip (performMove moveWithoutFlip)) stacks moves
 where
    (stacks, moves) = parseInput input