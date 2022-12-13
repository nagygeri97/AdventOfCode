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

data Signal
    = Value Int
    | List [Signal]
    deriving (Eq, Show)

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

parseListItems :: String -> ([Signal], String)
parseListItems (']':xs) = ([], xs)
parseListItems (',':xs) = parseListItems xs
parseListItems xs = (firstSignal : restSignals, finalRest) 
    where
        (firstSignal, rest) = parseSignal xs
        (restSignals, finalRest) = parseListItems rest


parseSignal :: String -> (Signal, String)
parseSignal ('[':xs) = (List listItems, rest)
    where
        (listItems, rest) = parseListItems xs
parseSignal xs = (Value (read numStr), rest)
    where
        (numStr, rest) = span isDigit xs

lineToSignal :: String -> Signal
lineToSignal = fst . parseSignal

convertInput :: [ParsedLine] -> [(Signal, Signal)]
convertInput = map (\[l,r] -> (lineToSignal l, lineToSignal r)) . splitBy []

compare :: Signal -> Signal -> Ordering
compare (Value n) (Value m) = Prelude.compare n m
compare (List []) (List []) = EQ
compare (List []) (List _) = LT
compare (List _) (List []) = GT
compare (List (x:xs)) (List (y:ys))
    | cmp == EQ = Main.compare (List xs) (List ys)
    | otherwise = cmp
    where
        cmp = Main.compare x y
compare (Value n) (List ys) = Main.compare (List [Value n]) (List ys)
compare (List xs) (Value m) = Main.compare (List xs) (List [Value m])

solve1 :: [ParsedLine] -> Int
solve1 = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry Main.compare) . convertInput

convertInput2 :: [ParsedLine2] -> [Signal]
convertInput2 = map lineToSignal . filter (not . null)

isDivider :: Signal -> Bool
isDivider (List [Value 2]) = True
isDivider (List [Value 6]) = True
isDivider _ = False

addDividers :: [Signal] -> [Signal]
addDividers xs = (List [Value 2]):(List [Value 6]):xs

solve2 :: [ParsedLine2] -> Int
solve2 = product . map fst . filter (isDivider . snd) . zip [1..] . sortBy Main.compare . addDividers . convertInput2