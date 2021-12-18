module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import System.IO
import Control.Applicative
import Data.Map (Map(..), (!), empty, insert, insertWith, foldrWithKey, elems)

type ParsedLine = String
type Rules = Map String Char

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

convert :: String -> ParsedLine
convert = id

parseInput :: [ParsedLine] -> (String, Rules)
parseInput input = (start, rulesMap)
    where
        [[start], rules] = splitBy [] input
        parsedRules = map ((\[from,_,[to]] -> (from, to)). words) rules
        rulesMap = foldr (uncurry Data.Map.insert) Data.Map.empty parsedRules

solve1 :: [ParsedLine] -> Int
solve1 input = (\x -> last x - head x) . sort . map length . group . sort $ lastStep 
    where
        (start, rules) = parseInput input
        lastStep = head $ drop 10 $ iterate (performStep rules) start

performStep :: Rules -> String -> String
performStep rules (x:y:xs) = x:(rules ! [x,y]):performStep rules (y:xs)
performStep rules xs = xs

solve2 :: [ParsedLine] -> Int
solve2 input = (last counts - head counts) `div` 2
    where
        (start, rules) = parseInput input
        startPairs = getPairs start
        lastStep = aggregatePairs $ head $ drop 40 $ iterate (performStep2 rules) startPairs
        correctLastStep = insertWith (+) (head start) 1 (insertWith (+) (last start) 1 lastStep)
        counts = sort $ elems $ correctLastStep

type PairsTable = Map String Int

getPairs :: String -> PairsTable
getPairs (x:y:xs) = insertWith (+) [x,y] 1 (getPairs (y:xs))
getPairs _ = Data.Map.empty

performStep2 :: Rules -> PairsTable -> PairsTable
performStep2 rules pairs = foldrWithKey (processPair rules) Data.Map.empty pairs

processPair :: Rules -> String -> Int -> PairsTable -> PairsTable
processPair rules key@[a,b] val result = insertWith (+) [a,c] val (insertWith (+) [c,b] val result)
    where
        c = rules ! key

type CharTable = Map Char Int

aggregatePairs :: PairsTable -> CharTable
aggregatePairs = foldrWithKey (\[a,b] val result -> insertWith (+) a val (insertWith (+) b val result)) Data.Map.empty