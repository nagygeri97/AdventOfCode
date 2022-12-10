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

data Op = Addx Int | Noop deriving (Eq, Show)
type ParsedLine = Op

type ParsedLine2 = ParsedLine

main :: IO ()
main = mainLoop [] []

mainLoop :: [ParsedLine] -> [ParsedLine2] -> IO ()
mainLoop xs ys = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                putStrLn "Part Two: " 
                solve2 xs
        else do line <- getLine
                mainLoop (xs ++ [convert line]) (ys ++ [convert2 line])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert line
    | instruction == "noop" = Noop
    | instruction == "addx" = Addx (read (splitLine !! 1))
    | otherwise = error "Invalid instruction"
    where 
        splitLine = splitBy ' ' line
        instruction = head splitLine

-- X register, result
type MachineState = (Int, [Int])

initialState :: MachineState
initialState = (1, [])

executeMachine :: [Op] ->  State MachineState [Int]
executeMachine [] = do
    (_, result) <- get
    return result
executeMachine (op:ops) = do
    (xBefore, accBefore) <- get
    modify (executeOperation op)
    (xAfter, _) <- get
    let accAfter = accBefore ++ addXStates op xBefore
    put (xAfter, accAfter) 
    executeMachine ops

executeOperation :: Op -> MachineState -> MachineState
executeOperation Noop (x, acc) = (x, acc)
executeOperation (Addx n) (x, acc) = (x + n, acc)

addXStates :: Op -> Int -> [Int]
addXStates op xBefore = replicate (len op) xBefore

len :: Op -> Int
len Noop = 1
len (Addx _) = 2

interestingCycles :: Int -> [Int] -> [Int]
interestingCycles _ [] = []
interestingCycles n (x:xs)
    | n `mod` 40 == 20 = n*x:interestingCycles (n + 1) xs
    | otherwise = interestingCycles (n + 1) xs

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 lines = sum $ interestingCycles 1 states
    where 
        states = evalState (executeMachine lines) initialState

drawCrt :: Int -> [Int] -> String
drawCrt _ [] = []
drawCrt n (x:xs)
    | abs (((n `mod` 40) - 1) - x) <= 1 = '█':drawCrt (n + 1) xs
    | otherwise = ' ':drawCrt (n + 1) xs

displayImage :: String -> IO ()
displayImage [] = return ()
displayImage s = do 
    putStrLn h 
    displayImage t
    where
        (h, t) = splitAt 40 s

solve2 :: [ParsedLine2] -> IO ()
solve2 lines = displayImage . drawCrt 1 $ states
    where 
        states = evalState (executeMachine lines) initialState