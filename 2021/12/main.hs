module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import System.IO
import Control.Applicative
import Data.Map

type ParsedLine = (String, String)

type Graph = Map String [String]

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
    | Prelude.null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert line = (x,y) 
    where 
        [x,y] = splitBy '-' line

convertToGraph :: [ParsedLine] -> Graph
convertToGraph [] = Data.Map.empty
convertToGraph ((x,y):xs) = insertWith (++) x [y] (insertWith (++) y [x] (convertToGraph xs))

solve1 :: [ParsedLine] -> Int
solve1 input = length $ contPath g ["start"]
    where
        g = convertToGraph input

isSmall :: String -> Bool
isSmall = isLower . head

contPath :: Graph -> [String] -> [[String]]
contPath g (p:path)
    | p == "end" = [p:path]
    | otherwise = concat $ Prelude.map (contPath g) newPaths 
    where
        validNeighbours = Prelude.filter (isValidInPath (p:path)) (g ! p)
        newPaths = [n:p:path | n<-validNeighbours]

isValidInPath :: [String] -> String -> Bool
isValidInPath path node = not $ isSmall node && elem node path

solve2 :: [ParsedLine] -> Int
solve2 input = length $ contPath2 g (False, ["start"])
    where
        g = convertToGraph input

contPath2 :: Graph -> (Bool, [String]) -> [(Bool, [String])]
contPath2 g (hasDouble, (p:path))
    | p == "end" = [(hasDouble, p:path)]
    | otherwise = concat $ Prelude.map (contPath2 g) newPaths 
    where
        validNeighbours = Prelude.filter (isValidInPath2 hasDouble (p:path)) (g ! p)
        newPaths = [(hasDouble || (isSmall n && elem n (p:path)), n:p:path) | n<-validNeighbours]

isValidInPath2 :: Bool -> [String] -> String -> Bool
isValidInPath2 hasDouble path node
    | node == "end" || node == "start" = isValidInPath path node
    | not $ isSmall node = True
    | otherwise = not hasDouble || (not . elem node $ path)