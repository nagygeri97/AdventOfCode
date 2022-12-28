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
import qualified Data.Set as Set

type ParsedLine = Costs 
-- (ore - ore, clay - ore, obsi - (ore, clay), geo - (ore, obsi))
type Costs = (Int, Int, (Int, Int), (Int, Int))

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
convert line = (oreOre, clayOre, (obsiOre, obsiClay), (geoOre, geoObsi))
    where
        [_, oreOre, clayOre, obsiOre, obsiClay, geoOre, geoObsi] = map read . filter (isDigit . head) . groupBy ((==) `on` isDigit) $ line

-- (time, qty@(ore, clay, obsi, geo), bot@(ore, clay, obsi, geo))
type S = (Int, (Int, Int, Int, Int), (Int, Int, Int, Int))

nextStates :: Costs -> S -> [S]
nextStates (oreOre, clayOre, (obsiOre, obsiClay), (geoOre, geoObsi)) (time, qty@(oreQ, clayQ, obsiQ, geoQ), bot@(oreB, clayB, obsiB, geoB)) = statesAfterNoBuy ++ statesAfterOreBuy ++ statesAfterClayBuy ++ statesAfterObsiBuy ++ statesAfterGeoBuy
    where
        newQ@(nOreQ, nClayQ, nObsiQ, nGeoQ) = (oreQ + oreB, clayQ + clayB, obsiQ + obsiB, geoQ + geoB)
        statesAfterNoBuy = [(time + 1, newQ, bot)]
        statesAfterOreBuy = if oreQ >= oreOre then [(time + 1, (nOreQ - oreOre, nClayQ, nObsiQ, nGeoQ), (oreB + 1, clayB, obsiB, geoB))] else []
        statesAfterClayBuy = if oreQ >= clayOre then [(time + 1, (nOreQ - clayOre, nClayQ, nObsiQ, nGeoQ), (oreB, clayB + 1, obsiB, geoB))] else []
        statesAfterObsiBuy = if oreQ >= obsiOre && clayQ >= obsiClay then [(time + 1, (nOreQ - obsiOre, nClayQ - obsiClay, nObsiQ, nGeoQ), (oreB, clayB, obsiB + 1, geoB))] else []
        statesAfterGeoBuy = if oreQ >= geoOre && obsiQ >= geoObsi then [(time + 1, (nOreQ - geoOre, nClayQ, nObsiQ - geoObsi, nGeoQ), (oreB, clayB, obsiB, geoB + 1))] else []

iterateStates :: Int -> Costs -> Int -> [S] -> [S]
iterateStates n costs time states 
    | time >= n = states
    | otherwise = iterateStates n costs (time + 1) newStates
    where 
        newStates = take 5000 . sortBy (flip compare `on` scoreState) . concat . map (nextStates costs) $ states

scoreState :: S -> (Int, Int, Int, Int, Int, Int, Int, Int)
scoreState (time, qty@(oreQ, clayQ, obsiQ, geoQ), bot@(oreB, clayB, obsiB, geoB)) = (geoB, geoQ, obsiB, obsiQ, clayB, clayQ, oreB, oreQ)

initialState :: S
initialState = (0, (0,0,0,0), (1,0,0,0))

getMaxGeo :: [S] -> Int
getMaxGeo = maximum . map (\(_, (_, _, _, g), _) -> g)

getWithMaxGeo :: [S] -> S
getWithMaxGeo = maximumBy (compare `on` (\(_, (_, _, _, g), _) -> g))

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 lines = sum . zipWith (*) [1..] . map (\cost -> getMaxGeo $ iterateStates 24 cost 0 [initialState]) $ lines 

solve2 :: [ParsedLine2] -> Int
solve2 lines = product . map (\cost -> getMaxGeo $ iterateStates 32 cost 0 [initialState]) . take 3 $ lines 