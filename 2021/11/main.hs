module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ArrayInt a = Array Int a

type ParsedLine = ArrayInt Int
type Arr = ArrayInt ParsedLine

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ listToArray xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ listToArray xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

listToArray :: [a] -> ArrayInt a
listToArray xs = listArray (0, length xs - 1) xs

convert :: String -> ParsedLine
convert = listToArray . map (\x -> read [x])

solve1 :: Arr -> Int
solve1 = sum . map fst . take (100+1) . simulate

simulate :: Arr -> [(Int, Arr)]
simulate arr = iterate singleStep (0, arr) 

singleStep :: (Int, Arr) -> (Int, Arr)
singleStep (_, arr) = flashAll incArr flashes 0
    where
        incArr = fmap (fmap (+1)) arr
        flashes = filter (\(x,y) -> incArr ! x ! y > 9) (indexes arr)
        
indexes :: Arr -> [(Int, Int)]
indexes arr = cross (indices arr) (indices (arr ! 0))

flashAll :: Arr -> [(Int, Int)] -> Int -> (Int, Arr)
flashAll arr [] n = (n, arr)
flashAll arr ((x,y):xs) n
    | arr ! x ! y > 9 = flashAll newArr (xs ++ newFlashes) (n + 1)
    | otherwise = flashAll arr xs n
    where
        ns = neighbours arr (x,y)
        newFlashes = filter (\(x,y) -> newArr ! x ! y > 9) ns
        tmpArr = arr // [(x, arr ! x // [(y,0)])]
        newArr = foldr (\(x,y) acc -> acc // [(x, acc ! x // [(y, condInc (acc ! x ! y))])]) tmpArr ns

condInc :: Int -> Int
condInc 0 = 0
condInc x = x + 1

cross :: [a] -> [b] -> [(a,b)]
cross = liftA2 (,) 

neighbours :: Arr -> (Int, Int) -> [(Int, Int)]
neighbours arr (x,y) = filter (\(x,y) -> x >= 0 && x <= maxx && y >= 0 && y <= maxy) 
    [(x-1,y), (x+1,y), (x,y-1), (x,y+1), (x+1, y-1), (x+1, y+1), (x-1, y-1), (x-1, y+1)]
    where
        (_, maxx) = bounds arr
        (_, maxy) = bounds (arr ! 0) 

solve2 :: Arr -> Int
solve2 arr = fst . head . dropWhile ((<size) . fst . snd) . zip [0..] . simulate $ arr
    where 
        size = length (indexes arr)