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
type Cube = (Point, Point)

type ParsedLine = Step

type Switch = (OnOff, Cube)

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
solve2 lines = volume
    where
        switches = map transformStep lines  
        allDoneSwitches = performAllSwitches switches
        onCubes = filter (\(x,_) -> x == On) allDoneSwitches
        volume = sum . map (vol . snd) $ onCubes


transformStep :: Step -> Switch
transformStep (onoff, (x1,x2), (y1,y2), (z1,z2)) = (onoff, ((x1,y1,z1), (x2,y2,z2)))

intersectCube :: Cube -> Cube -> Maybe Cube
intersectCube ((x1,y1,z1), (x2,y2,z2)) ((a1,b1,c1), (a2,b2,c2))
    | ax1 > ax2 || by1 > by2 || cz1 > cz2 = Nothing
    | otherwise = Just ((ax1,by1,cz1), (ax2,by2,cz2))
    where
        ax1 = max x1 a1
        by1 = max y1 b1
        cz1 = max z1 c1
        ax2 = min x2 a2
        by2 = min y2 b2
        cz2 = min z2 c2

isValid :: Cube -> Bool
isValid ((x1,y1,z1), (x2,y2,z2)) = x1 <= x2 && y1 <= y2 && z1 <= z2

difference :: Cube -> Cube -> [Cube]
difference first@((x1,y1,z1), (x2,y2,z2)) second
    | isNothing common = [first]
    | otherwise = filter isValid [top, bottom, right, left, back, front]
    where
        common = intersectCube first second
        ((a1,b1,c1), (a2,b2,c2)) = fromJust common

        top = ((a2 + 1, y1, z1), (x2, y2, z2))
        bottom = ((x1, y1, z1), (a1 - 1, y2, z2))

        right = ((a1, b2 + 1, z1), (a2, y2, z2))
        left = ((a1, y1, z1), (a2, b1 - 1, z2))

        back = ((a1, b1, c2 + 1), (a2, b2, z2))
        front = ((a1, b1, z1), (a2, b2, c1 - 1))

getDiffToList :: Cube -> [Cube] -> [Cube]
getDiffToList cube = foldr (\x acc -> concatMap (`difference` x) acc) [cube]

performSwitch :: Switch -> [Switch] -> [Switch]
performSwitch (onoff, cube) switches = map ((,) onoff) (getDiffToList cube cubes) ++ switches
    where
        (_, cubes) = unzip switches

performAllSwitches :: [Switch] -> [Switch]
performAllSwitches [] = []
performAllSwitches (s:ss) = performSwitch s (performAllSwitches ss)

vol :: Cube -> Int
vol ((x1,y1,z1), (x2,y2,z2)) = (x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)