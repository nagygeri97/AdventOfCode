module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import System.IO
import Control.Applicative
import Data.Array as A

type ParsedLine = String
type ArrayInt a = Array Int a
type Image = ArrayInt (ArrayInt Char)
type FullImage = (Char, Image) 
type Key = ArrayInt Char

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do let input = parseInput xs
                putStrLn . ("Part One: " ++) . show . solve1 $ input
                putStrLn . ("Part Two: " ++) . show . solve2 $ input
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

listToArray :: [a] -> ArrayInt a
listToArray xs = listArray (0, length xs - 1) xs

listListToArray :: [[a]] -> ArrayInt (ArrayInt a)
listListToArray = listToArray . map listToArray

parseInput :: [String] -> (Key, FullImage)
parseInput input = (keyArr, ('.', imgArr))
    where
        [[key], image] = splitBy [] input
        keyArr = listToArray key
        imgArr = listListToArray image

stringToIndex :: String -> Int
stringToIndex = foldl (\acc x -> 2*acc + fromEnum (x == '#')) 0

neighbours :: FullImage -> (Int, Int) -> String
neighbours (def, img) (x, y) = map (getWithDefault img def) [(xx,yy) | xx <- [(x-1)..(x+1)], yy <- [(y-1)..(y+1)]]
    
getWithDefault :: Image -> Char -> (Int, Int) -> Char
getWithDefault img def (x, y)
    | x >= 0 && y >= 0 && x <= maxx && y <= maxy = img ! x ! y
    | otherwise = def
    where
        (_,maxx) = bounds img
        (_,maxy) = bounds $ img ! 0

getAllIndexes :: Image -> [[(Int, Int)]]
getAllIndexes img = [zip [x,x..] [-1..maxy+1] | x <- [-1..maxx+1]]
    where
        (_,maxx) = bounds img
        (_,maxy) = bounds $ img ! 0

generateNewImage :: FullImage -> Key -> FullImage 
generateNewImage full@(def, img) key = (newDef, newImgArr)
    where
        indices = getAllIndexes img
        newImg = (map . map) ((key!) . stringToIndex . neighbours full) indices
        newImgArr = listListToArray newImg
        newDef = key ! (stringToIndex $ replicate 9 def)

countLights :: FullImage -> Int
countLights (def, img) = length . filter (=='#') . concat . map elems $ (elems img)

applyNTimes :: Int -> Key -> FullImage -> FullImage
applyNTimes n _ full | n <= 0 = full
applyNTimes n key full = applyNTimes (n-1) key (generateNewImage full key)


solve1 :: (Key, FullImage) -> Int
solve1 (key, full) = countLights $ applyNTimes 2 key full

solve2 :: (Key, FullImage) -> Int
solve2 (key, full) = countLights $ applyNTimes 50 key full