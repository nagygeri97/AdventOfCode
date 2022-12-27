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

type ParsedLine = Cube
type Cube = (Int, Int, Int)

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
convert = (\[a,b,c] -> (a,b,c)) . map read . splitBy ','

convert2 :: String -> ParsedLine2
convert2 = convert

neighbors :: Cube -> [Cube]
neighbors (a,b,c) = [(a,b,c-1), (a,b,c+1), (a,b-1,c), (a,b+1,c), (a-1,b,c), (a+1,b,c)]

solve1 :: [ParsedLine] -> Int
solve1 cubes = sum . map (length . filter (\n -> not $ Set.member n cubesSet) . neighbors) $ cubes
    where
        cubesSet = Set.fromList cubes

extremes :: [Cube] -> (Cube, Cube)
extremes [x] = (x,x)
extremes ((a,b,c):xs) = ((min a minx, min b miny, min c minz), (max a maxx, max b maxy, max c maxz))
    where
        ((minx, miny, minz), (maxx, maxy, maxz)) = extremes xs

extendBounds :: (Cube, Cube) -> (Cube, Cube)
extendBounds ((minx, miny, minz), (maxx, maxy, maxz)) = ((minx - 1, miny - 1, miny - 1), (maxx + 1, maxy + 1, maxz + 1))

fill :: Set.Set Cube -> (Cube, Cube) -> Cube -> Set.Set Cube -> Set.Set Cube
fill cubes bounds@((minx, miny, minz), (maxx, maxy, maxz)) current@(x, y, z) result
    | Set.member current result = result
    | x < minx || y < miny || z < minz || x > maxx || y > maxy || z > maxz = result
    | Set.member current cubes = result 
    | otherwise = backResult
    where
        newResult = Set.insert current result
        upResult = fill cubes bounds (x + 1, y, z) newResult
        downResult = fill cubes bounds (x - 1, y, z) upResult
        rightResult = fill cubes bounds (x, y + 1, z) downResult
        leftResult = fill cubes bounds (x, y - 1, z) rightResult
        frontResult = fill cubes bounds (x, y, z + 1) leftResult 
        backResult = fill cubes bounds (x, y, z - 1) frontResult

neighborsInBounds :: (Cube, Cube) -> Cube -> [Cube]
neighborsInBounds ((minx, miny, minz), (maxx, maxy, maxz)) (a,b,c) = filter (\(x,y,z) -> x >= minx && x <= maxx && y >= miny && y <= maxy && z >= minz && z <= maxz) [(a,b,c-1), (a,b,c+1), (a,b-1,c), (a,b+1,c), (a-1,b,c), (a+1,b,c)]

solve2 :: [ParsedLine2] -> Int
solve2 cubes = sum . map (length . filter (\n -> not $ Set.member n boundingCubesSet) . neighborsInBounds bounds) $ boundingCubes
    where
        boundingCubes = Set.toList boundingCubesSet
        boundingCubesSet = fill cubeSet bounds (fst bounds) Set.empty
        bounds = extendBounds . extremes $ cubes
        cubeSet = Set.fromList cubes