module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type Area = ((Int, Int), (Int, Int))
type ParsedLine = Area

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ head xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ head xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert line = ((x1, x2), (y1, y2))
    where
        [x1,x2,y1,y2] = map read . filter (num . head) . groupBy ((==) `on` num) $ line

        num :: Char -> Bool
        num x = isDigit x || x == '-'

inArea :: (Int, Int) -> Area -> Bool
inArea (px, py) ((minx, maxx), (miny, maxy)) = px >= minx && px <= maxx && py >= miny && py <= maxy

outArea :: (Int, Int) -> Area -> Bool
outArea (px, py) ((minx, maxx), (miny, maxy)) = px > maxx || py < miny

simulateShot :: Area -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
simulateShot area pos@(px, py) (vx, vy)
    | pos `inArea` area = Just [pos]
    | pos `outArea` area = Nothing
    | isJust rest = Just $ pos : fromJust rest
    | otherwise = Nothing 
    where 
        rest = simulateShot area (px + vx, py + vy)  (nvx, nvy) 
        nvy = vy - 1
        nvx
            | vx < 0 = vx + 1
            | vx > 0 = vx - 1
            | otherwise = vx

velocities :: Area -> [(Int, Int)]
velocities ((minx, maxx), (miny, maxy)) = [(vx, vy) | vx <- [1..maxx], vy <- [miny..abs miny]]

solve1 :: Area -> Int
solve1 area = maxY
    where
        vs = velocities area
        start = (0,0)
        paths = map fromJust . filter isJust . map (simulateShot area start) $ vs
        maxY = maximum . map (maximum . map snd) $ paths 

solve2 :: ParsedLine -> Int
solve2 area = length paths
    where
        vs = velocities area
        start = (0,0)
        paths = filter isJust . map (simulateShot area start) $ vs