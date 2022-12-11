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

data Operand = Old | Value Int deriving (Eq, Show)
data Operator = Add | Mul deriving (Eq, Show)

data Operation = Op {
    left :: Operand,
    op :: Operator,
    roight :: Operand
} deriving (Eq, Show)

data Monkey = Monkey {
    items :: [Int],
    operation :: Operation,
    testDivBy :: Int,
    ifTrueThrowTo :: Int,
    ifFalseThrowTo :: Int,
    inspected :: Int
} deriving (Eq, Show)

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

parseItems :: String -> [Int]
parseItems = map read . filter (not . null) . map (filter isDigit) . splitBy ' '

parseOperand :: String -> Operand
parseOperand "old" = Old
parseOperand op = Value (read op)

parseOperator :: String -> Operator
parseOperator "+" = Add
parseOperator "*" = Mul
parseOperator _ = error "Unknown operator"

parseOp :: String -> Operation
parseOp line = Op (parseOperand left) (parseOperator op) (parseOperand right) 
    where
        [left, op, right] = take 3 . reverse . splitBy ' ' $ line

parseTest :: String -> Int
parseTest = read . last . splitBy ' '

parseThrowTo :: String -> Int
parseThrowTo = read . last . splitBy ' '

parseMonkey :: [String] -> Monkey
parseMonkey lines = Monkey (parseItems itemsLine) (parseOp opLine) (parseTest testLine) (parseThrowTo trueLine) (parseThrowTo falseLine) 0
    where
        [header, itemsLine, opLine, testLine, trueLine, falseLine] = lines

monkeysToArray :: [Monkey] -> Array Int Monkey
monkeysToArray ms = listArray (0, length ms - 1) ms

getMonkeys :: [ParsedLine] -> Array Int Monkey
getMonkeys = monkeysToArray . map parseMonkey . splitBy []

addItemToMonkey :: Monkey -> Int -> Monkey
addItemToMonkey (Monkey items op test true false inspected) item = Monkey (items ++ [item]) op test true false inspected

simulateOneStep :: Int -> Array Int Monkey -> Bool -> Int -> Array Int Monkey
simulateOneStep n ms div3 mo
    | n > (snd $ bounds ms) = ms
    | otherwise = simulateOneStep (n + 1) ((foldl (\arr (w, t) -> arr // [(t, addItemToMonkey (arr ! t) w)]) ms newItemsWithThrowTo) // [(n, Monkey [] op test true false (inspected + length items))]) div3 mo
    where
        Monkey items op test true false inspected = ms ! n
        newItems = map ((if div3 then (`div` 3) else id) . performOp op mo) items
        throwTo = map ((\x -> if x then true else false) . (==0) . (`mod` test)) newItems
        newItemsWithThrowTo = zip newItems throwTo

simulateNSteps :: Int -> Array Int Monkey -> Bool -> Int -> Array Int Monkey
simulateNSteps 0 ms _ _ = ms
simulateNSteps n ms div3 mo = simulateNSteps (n - 1) (simulateOneStep 0 ms div3 mo) div3 mo

performOp :: Operation -> Int -> Int -> Int
performOp (Op left op right) mo old = calculateOp op leftValue rightValue mo
    where
        leftValue = getValue left old
        rightValue = getValue right old

getValue :: Operand -> Int -> Int
getValue Old old = old
getValue (Value value) _ = value

calculateOp :: Operator -> Int -> Int -> Int -> Int
calculateOp Add n m mo = (n + m) `mod` mo
calculateOp Mul n m mo = (n * m) `mod` mo

convert :: String -> ParsedLine
convert = id

convert2 :: String -> ParsedLine2
convert2 = convert

getMo :: Array Int Monkey -> Int
getMo = product . map testDivBy . elems

solve1 :: [ParsedLine] -> Int
solve1 lines = product . map inspected . take 2 . sortBy (flip compare `on` inspected) . elems $ monkeysAfter20Steps
    where
        monkeysAfter20Steps = simulateNSteps 20 monkeys True (getMo monkeys)
        monkeys = getMonkeys lines

solve2 :: [ParsedLine2] -> Int
solve2 lines = product . map inspected . take 2 . sortBy (flip compare `on` inspected) . elems $ monkeysAfter10000Steps
    where
        monkeysAfter10000Steps = simulateNSteps 10000 monkeys False (getMo monkeys)
        monkeys = getMonkeys lines