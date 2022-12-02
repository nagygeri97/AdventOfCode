module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = [String]

data RPS = Rock | Paper | Scissors deriving (Eq, Show)

data Goal = Lose | Draw | Win deriving (Eq, Show)

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

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

parseFst :: String -> RPS
parseFst "A" = Rock
parseFst "B" = Paper
parseFst "C" = Scissors

parseSnd :: String -> RPS
parseSnd "X" = Rock
parseSnd "Y" = Paper
parseSnd "Z" = Scissors

convert :: String -> ParsedLine
convert = splitBy ' '

score :: (RPS, RPS) -> Int
score p@(_, s) = winScore p + moveScore s

winScore :: (RPS, RPS) -> Int
winScore (x, y)
 | x == y = 3
winScore (Rock, Paper) = 6
winScore (Paper, Scissors) = 6
winScore (Scissors, Rock) = 6
winScore _ = 0

moveScore :: RPS -> Int
moveScore Rock = 1
moveScore Paper = 2
moveScore Scissors = 3

convertToFirst :: [String] -> (RPS, RPS)
convertToFirst [fst, snd] = (parseFst fst, parseSnd snd)

convertToSecond :: [String] -> (RPS, Goal)
convertToSecond [fst, snd] = (parseFst fst, parseGoal snd)

parseGoal :: String -> Goal
parseGoal "X" = Lose
parseGoal "Y" = Draw
parseGoal "Z" = Win

getSymbolForOutcome :: (RPS, Goal) -> RPS
getSymbolForOutcome (x, Draw) = x
getSymbolForOutcome (Rock, Win) = Paper
getSymbolForOutcome (Rock, Lose) = Scissors
getSymbolForOutcome (Paper, Win) = Scissors
getSymbolForOutcome (Paper, Lose) = Rock
getSymbolForOutcome (Scissors, Win) = Rock
getSymbolForOutcome (Scissors, Lose) = Paper

solve1 :: [ParsedLine] -> Int
solve1 = sum . map (score . convertToFirst)

solve2 :: [ParsedLine] -> Int
solve2 = sum . map (score . (\p@(x, g) -> (x, getSymbolForOutcome p)) . convertToSecond)