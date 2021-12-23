module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

data OnOff
    = On
    | Off
    deriving (Eq, Show)

type Point = (Int, Int, Int)
type Range = (Int, Int)
type Step = (OnOff, Range, Range, Range)

type ParsedLine = Step

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ xs
                -- putStrLn . ("Part Two: " ++) . show . solve2 $ xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert line = (onoff, (x1,x2), (y1,y2), (z1,z2))
    where
        (o:_) = words line
        onoff
            | o == "on" = On
            | otherwise = Off

        numFun = (\x -> isDigit x || x == '-')
        [x1,x2,y1,y2,z1,z2] = map read . filter (numFun . head) . groupBy ((==) `on` numFun) $ line

isOn :: [Step] -> Point -> Bool
isOn s p 
    | null insides = False 
    | otherwise = (==On) . (\(x,_,_,_) -> x) . last $ insides
    where
        insides = filter (insideRange p) s

insideRange :: Point -> Step -> Bool
insideRange (a,b,c) (change, x, y, z) = inside a x && inside b y && inside c z 

inside :: Int -> Range -> Bool
inside a (x,y) = x <= a && a <= y

coords :: [Point]
coords = [(x,y,z) | x <- [-50..50], y <- [-50..50], z <- [-50..50]]

solve1 :: [ParsedLine] -> Int
solve1 lines = length . filter (isOn lines) $ coords

solve2 :: [ParsedLine] -> Int
solve2 = undefined

-- intersect max a kezdo, min a vegso range ertekkel
-- hatulrol dolgozzuk fel, taroljuk mindenhez hogy mennyit affectel
-- ha jon egy korabbi osszeintersectelunk 
-- https://docs.google.com/spreadsheets/d/1QdVNqHF4Kv_hVrNpTZVIZ83DNrxl8_7JugUcT9PLhPU/edit#gid=0
-- az intersecteket is kell intersectelni, hogy jo legyen