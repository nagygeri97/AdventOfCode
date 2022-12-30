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
import qualified Data.Map as Map

data Op = Add | Sub | Mul | Div | Equal deriving (Eq, Show)
data Monkey
    = N Int
    | O String Op String
    | H
    deriving (Eq, Show)
type ParsedLine = (String, Monkey)

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

readOp :: String -> Op
readOp "+" = Add
readOp "-" = Sub
readOp "*" = Mul
readOp "/" = Div

convert :: String -> ParsedLine
convert line
    | length rest == 1 = (init name, N . read . head $ rest)
    | otherwise = (init name, O (rest !! 0) (readOp $ rest !! 1) (rest !! 2) )
    where
        (name:rest) = splitBy ' ' line

convert2 :: String -> ParsedLine2
convert2 = convert

performOp :: Op -> Int -> Int -> Int
performOp Add = (+)
performOp Sub = (-)
performOp Mul = (*)
performOp Div = div

evaluate :: Map.Map String Monkey -> String -> Int
evaluate monkeys label = value
    where
        monkey = monkeys Map.! label
        value = case monkey of
            N v -> v
            O l1 op l2 -> performOp op (evaluate monkeys l1) (evaluate monkeys l2)

solve1 :: [ParsedLine] -> Int
solve1 lines = evaluate monkeys "root"
    where
        monkeys = Map.fromList lines

evaluateNoHuman :: Map.Map String Monkey -> String -> Maybe Int
evaluateNoHuman monkeys label = value
    where
        monkey = monkeys Map.! label
        value = case monkey of
            N v -> Just v
            H -> Nothing
            O l1 op l2 -> performOpMaybe op (evaluateNoHuman monkeys l1) (evaluateNoHuman monkeys l2)

performOpMaybe :: Op -> Maybe Int -> Maybe Int -> Maybe Int
performOpMaybe = liftM2 . performOp

getNewResult :: Int -> Maybe Int -> Maybe Int -> Op -> Int
getNewResult result Nothing (Just n) Add = result - n
getNewResult result (Just n) Nothing Add = result - n
getNewResult result Nothing (Just n) Sub = result + n
getNewResult result (Just n) Nothing Sub = n - result
getNewResult result Nothing (Just n) Mul = result `div` n
getNewResult result (Just n) Nothing Mul = result `div` n
getNewResult result Nothing (Just n) Div = result * n
getNewResult result (Just n) Nothing Div = n `div` result
getNewResult result Nothing (Just n) Equal = n
getNewResult result (Just n) Nothing Equal = n

neededForResult :: Map.Map String Monkey -> Int -> String -> Int
neededForResult monkeys result label
    | label == "humn" = result
    | isNothing leftVal = neededForResult monkeys newResult l1
    | otherwise = neededForResult monkeys newResult l2
    where
        (O l1 op l2) = monkeys Map.! label
        leftVal = evaluateNoHuman monkeys l1
        rightVal = evaluateNoHuman monkeys l2
        newResult = getNewResult result leftVal rightVal op

solve2 :: [ParsedLine2] -> Int
solve2 lines = neededForResult monkeys 0 "root"
    where
        monkeys = Map.insert "humn" H (Map.update (\(O l1 _ l2) -> Just (O l1 Equal l2)) "root" (Map.fromList lines))