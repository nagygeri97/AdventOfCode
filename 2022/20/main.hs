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

type ParsedLine = Int

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
convert = read

convert2 :: String -> ParsedLine2
convert2 = convert

indexOf :: Eq a => a -> [a] -> Int
indexOf e (x:xs)
    | e == x = 0
    | otherwise = 1 + indexOf e xs

insertRemove :: Int -> Int -> a -> [a] -> [a]
insertRemove = insertRemove' 0
    where
        insertRemove' :: Int -> Int -> Int -> a -> [a] -> [a]
        insertRemove' n insertIdx removeIdx e []
            | insertIdx == n = [e]
            | otherwise = []
        insertRemove' n insertIdx removeIdx e (x:xs)
            | n > insertIdx && n > removeIdx = (x:xs)
            | n == insertIdx && n == removeIdx = e:xs
            | n == insertIdx = e : x : insertRemove' (n+1) insertIdx removeIdx e xs
            | n == removeIdx = insertRemove' (n+1) (insertIdx+1) removeIdx e xs
            | otherwise = x : insertRemove' (n+1) insertIdx removeIdx e xs

unmixN :: Int -> ([Int], [Int]) -> Int -> ([Int], [Int])
unmixN len (currIdxToOrigIdx, currIdxToValues) n
    | currValue == 0 = (currIdxToOrigIdx, currIdxToValues)
    | otherwise = (insertionFn n currIdxToOrigIdx, insertionFn currValue currIdxToValues)
    where
        currIdx = indexOf n currIdxToOrigIdx
        currValue = currIdxToValues !! currIdx
        newIdx = (currIdx + currValue) `mod` (len - 1)
        insertionFn = insertRemove newIdx currIdx

solve1 :: [ParsedLine] -> Int
solve1 currIdxToValues = neededValue
    where
        len = length currIdxToValues
        currIdxToOrigIdx = [0..(len - 1)]
        (_, values) = foldl (unmixN len) (currIdxToOrigIdx, currIdxToValues) [0..(len - 1)]
        idxOfZero = indexOf 0 values
        neededIdxs = map (\idx -> (idx + idxOfZero) `mod` len)$ [1000, 2000, 3000]
        neededValue = sum . map (values !!) $ neededIdxs

solve2 :: [ParsedLine2] -> Int
solve2 lines = neededValue
    where
        currIdxToValues = map (*811589153) lines
        len = length currIdxToValues
        currIdxToOrigIdx = [0..(len - 1)]
        (_, values) = foldl (unmixN len) (currIdxToOrigIdx, currIdxToValues) . concat . replicate 10 $ [0..(len - 1)]
        idxOfZero = indexOf 0 values
        neededIdxs = map (\idx -> (idx + idxOfZero) `mod` len)$ [1000, 2000, 3000]
        neededValue = sum . map (values !!) $ neededIdxs