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
import qualified Data.Map as Map
import qualified Data.PQueue.Min as PQueue

type Label = String
data Node = Node {
    label :: Label,
    neighbors :: Set.Set Label,
    rate :: Int, 
    dist :: Int,
    opens :: Set.Set Label
} deriving (Eq, Show)

type Graph = Array Int GraphLevel
type GraphLevel = Map.Map Label Node

type ParsedLine = Node

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

removeComma :: String -> Label
removeComma [] = []
removeComma (',':xs) = xs
removeComma (x:xs) = x:removeComma xs

convert :: String -> ParsedLine
convert line = Node label neighborsSet rate 1 Set.empty
    where
        neighborsSet = Set.fromList $ map removeComma neighbors
        rate = read $ filter isDigit rateStr
        (_:label:_:_:rateStr:_:_:_:_:neighbors) = splitBy ' ' line

convertToGraphLevel :: [Node] -> GraphLevel -> GraphLevel
convertToGraphLevel [] g = g
convertToGraphLevel (n@(Node l _ _ _ _):ns) g = convertToGraphLevel ns (Map.insert l n g)

nextLevel :: GraphLevel -> (GraphLevel, Int, Set.Set Label) -> [(GraphLevel, Int, Set.Set Label)]
nextLevel originalG (g, clock, openLabs)  = (newGWithoutOpen, clock + 1, openLabs):openGs
    where
        levelElems = Map.elems g
        availableNodes = filter ((<=0) . dist) levelElems

        availableNeighbors :: [(Label, Int, Set.Set Label)]
        availableNeighbors = concat $ map (\(Node _ ns _ dist opens) -> map (\l -> (l, dist, opens)) (Set.toList ns)) availableNodes

        getAllNeighborsWithData :: Map.Map Label (Int, Set.Set Label) -> [(Label, Int, Set.Set Label)] -> Map.Map Label (Int, Set.Set Label)
        getAllNeighborsWithData m [] = m
        getAllNeighborsWithData m ((l, d, o):ns) = getAllNeighborsWithData (Map.insertWith min l (d, o) m) ns

        allNeighborsWithData = getAllNeighborsWithData Map.empty availableNeighbors

        closedAvailableNodes = filter (\n -> (not $ Set.member (label n) (opens n)) && rate n > 0) availableNodes

        openMoves = map (\node -> (label node, (clock + 1 - 30) * (rate node) + (dist node), Set.insert (label node) (opens node))) closedAvailableNodes
        openGs = map (\(lab, dist, ops) -> (Map.update (\(Node l n r _ _) -> Just $ Node l n r dist ops) lab originalG, clock + 1, ops)) openMoves

        newGWithoutOpen = foldr (\(lab, (dist, opens)) m -> Map.update (\(Node l n r _ _) -> Just $ Node l n r dist opens) lab m) originalG (Map.toList allNeighborsWithData)

runLevels :: GraphLevel -> [(GraphLevel, Int, Set.Set Label)] -> Int -> [(GraphLevel, Int, Set.Set Label)]
runLevels originalG currentLevels n
    | n > 30 = currentLevels
    | otherwise = runLevels originalG dedupedGraphs (n + 1)
    where
        nextLevels = concat $ map (nextLevel originalG) currentLevels

        deduplicate :: Map.Map (Set.Set Label) (GraphLevel, Int) -> [(GraphLevel, Int, Set.Set Label)] -> [(GraphLevel, Int, Set.Set Label)]
        deduplicate m [] = map (\(ops, (g, c)) -> (g, c, ops)) $ Map.toList m
        deduplicate m ((g, c, ops):xs) = deduplicate (Map.insertWith (\(g1, c1) (g2, c2) -> (mergeGraphs g1 g2, c1)) ops (g, c) m) xs

        mergeGraphs :: GraphLevel -> GraphLevel -> GraphLevel
        mergeGraphs = Map.unionWith (\n1 n2 -> if dist n1 < dist n2 then n1 else n2)

        dedupedGraphs = deduplicate Map.empty nextLevels

convert2 :: String -> ParsedLine2
convert2 = convert

choose :: Int -> [a] -> [[a]]
choose 0 _ = [[]]
choose n [] = []
choose n (x:xs) = [x:cs | cs <- choose (n-1) xs] ++ choose n xs

solve1 :: [ParsedLine] -> Int
solve1 lines = (*(-1)) . minimum . map dist . concat . map (\(a,_,_) -> Map.elems a) $ runLevels gLevel [firstLevelInfo] 0
    where
        gLevel = convertToGraphLevel lines Map.empty
        firstLevel = Map.update (\(Node l n r d o) -> Just $ Node l n r 0 o) "AA" $ gLevel
        firstLevelInfo = (firstLevel, 0, Set.empty)

solve2 :: [ParsedLine2] -> Int
solve2 lines = undefined