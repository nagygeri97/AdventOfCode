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

runLevels :: GraphLevel -> [(GraphLevel, Int, Set.Set Label)] -> Int -> [[(GraphLevel, Int, Set.Set Label)]]
runLevels originalG currentLevels n
    | n > 30 = [currentLevels]
    | otherwise = dedupedGraphs : runLevels originalG dedupedGraphs (n + 1)
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
solve1 = (*(-1)) . minimum . map dist . concat . map (\(a,_,_) -> Map.elems a) . last . doRunLevel

doRunLevel :: [ParsedLine] -> [[(GraphLevel, Int, Set.Set Label)]]
doRunLevel lines = runLevels gLevel [firstLevelInfo] 0
    where
        gLevel = convertToGraphLevel lines Map.empty
        firstLevel = Map.update (\(Node l n r d o) -> Just $ Node l n r 0 o) "AA" $ gLevel
        firstLevelInfo = (firstLevel, 0, Set.empty)

data Graph2Node = G2N {
    node1 :: Node,
    node2 :: Node,
    distance :: Int,
    ops :: Set.Set Label
} 
type GraphLevel2 = Map.Map (Label, Label) Graph2Node

crossProduct :: [a] -> [a] -> [(a, a)]
crossProduct xs ys = liftA2 (,) xs ys

convertToGraphLevel2 :: [(Node, Node)] -> GraphLevel2 -> GraphLevel2
convertToGraphLevel2 [] g = g 
convertToGraphLevel2 ((n1, n2):ns) g = convertToGraphLevel2 ns (Map.insert (label n1, label n2) (G2N n1 n2 1 Set.empty) g)

nextLevel2 :: GraphLevel2 -> (GraphLevel2, Int, Set.Set Label) -> [(GraphLevel2, Int, Set.Set Label)]
nextLevel2 originalG (g, clock, openLabs)  = [(newGWithoutOpen, clock + 1, openLabs)] ++ openGsFirst ++ openGsSecond ++ twoOpenGs
    where
        levelElems :: [Graph2Node]
        levelElems = Map.elems g

        availableNodes :: [Graph2Node]
        availableNodes = filter ((<=0) . distance) levelElems

        availableNeighbors :: [((Label,Label), Int, Set.Set Label)]
        availableNeighbors = concat $ map (\(G2N n1 n2 d ops) -> map (\l -> (l, d, ops)) (crossProduct (Set.toList $ neighbors n1) (Set.toList $ neighbors n2))) availableNodes

        getAllNeighborsWithData :: Map.Map (Label, Label) (Int, Set.Set Label) -> [((Label, Label), Int, Set.Set Label)] -> Map.Map (Label, Label) (Int, Set.Set Label)
        getAllNeighborsWithData m [] = m
        getAllNeighborsWithData m ((l, d, o):ns) = getAllNeighborsWithData (Map.insertWith min l (d, o) m) ns

        allNeighborsWithData :: Map.Map (Label, Label) (Int, Set.Set Label)
        allNeighborsWithData = getAllNeighborsWithData Map.empty availableNeighbors

        newGWithoutOpen :: GraphLevel2
        newGWithoutOpen = foldr (\(lab, (dist, opens)) m -> Map.update (\(G2N n1 n2 _ _) -> Just $ G2N n1 n2 dist opens) lab m) originalG (Map.toList allNeighborsWithData)

        closedAvailableNodesFirst :: [Graph2Node]
        closedAvailableNodesFirst = filter (\(G2N n _ _ ops) -> (not $ Set.member (label n) ops) && rate n > 0) availableNodes

        closedAvailableNodesSecond :: [Graph2Node]
        closedAvailableNodesSecond = filter (\(G2N _ n _ ops) -> (not $ Set.member (label n) ops) && rate n > 0) availableNodes

        oneOpenMovesFirst :: [((Label, Label), Int, Set.Set Label)]
        oneOpenMovesFirst = map (\(G2N n1 n2 d o) -> ((label n1, label n2), (clock + 1 - 26) * (rate n1) + d, Set.insert (label n1) o)) closedAvailableNodesFirst

        openMovesFirstWithStep :: [[((Label, Label), Int, Set.Set Label)]]
        openMovesFirstWithStep = map (\((l1, l2), d, ops) -> [((l1, n), d, ops) | n <- Set.toList (neighbors (node2 (g Map.! (l1, l2))))]) oneOpenMovesFirst

        oneOpenMovesSecond :: [((Label, Label), Int, Set.Set Label)]
        oneOpenMovesSecond = map (\(G2N n1 n2 d o) -> ((label n1, label n2), (clock + 1 - 26) * (rate n2) + d, Set.insert (label n2) o)) closedAvailableNodesSecond

        openMovesSecondWithStep :: [[((Label, Label), Int, Set.Set Label)]]
        openMovesSecondWithStep = map (\((l1, l2), d, ops) -> [((n, l2), d, ops) | n <- Set.toList (neighbors (node1 (g Map.! (l1, l2))))]) oneOpenMovesSecond

        openGsFirst :: [(GraphLevel2, Int, Set.Set Label)]
        openGsFirst = map (foldr (\(lab, dist, opens) (m, c, ops) -> (Map.update (\(G2N n1 n2 _ _) -> Just $ G2N n1 n2 dist opens) lab m, c, opens)) (originalG, clock + 1, Set.empty)) openMovesFirstWithStep

        openGsSecond :: [(GraphLevel2, Int, Set.Set Label)]
        openGsSecond = map (foldr (\(lab, dist, opens) (m, c, ops) -> (Map.update (\(G2N n1 n2 _ _) -> Just $ G2N n1 n2 dist opens) lab m, c, opens)) (originalG, clock + 1, Set.empty)) openMovesSecondWithStep

        closedAvailableNodesBoth :: [Graph2Node]
        closedAvailableNodesBoth = filter (\(G2N n1 n2 _ ops) -> ((label n1) /= (label n2) && (not $ Set.member (label n1) ops) && (not $ Set.member (label n2) ops) && (rate n1) > 0 && (rate n2) > 0)) availableNodes

        twoOpenMoves :: [((Label, Label), Int, Set.Set Label)]
        twoOpenMoves =  map (\(G2N n1 n2 d o) -> ((label n1, label n2), (clock + 1 - 26) * ((rate n1) + (rate n2)) + d, Set.insert (label n2) (Set.insert (label n1) o))) closedAvailableNodesBoth

        twoOpenGs :: [(GraphLevel2, Int, Set.Set Label)]
        twoOpenGs = map (\(lab, dist, ops) -> (Map.update (\(G2N n1 n2 d op) -> Just $ G2N n1 n2 dist ops) lab originalG, clock + 1, ops)) twoOpenMoves


runLevels2 :: GraphLevel2 -> [(GraphLevel2, Int, Set.Set Label)] -> Int -> Int -> Array Int Int-> [(GraphLevel2, Int, Set.Set Label)]
runLevels2 originalG currentLevels n openableCount scpl
    | n > 26 = currentLevels
    | otherwise = allOpenGraphs ++ runLevels2 originalG keepGoods (n + 1) openableCount scpl
    where
        nextLevels = concat $ map (nextLevel2 originalG) currentLevels

        deduplicate :: Map.Map (Set.Set Label) (GraphLevel2, Int) -> [(GraphLevel2, Int, Set.Set Label)] -> [(GraphLevel2, Int, Set.Set Label)]
        deduplicate m [] = map (\(ops, (g, c)) -> (g, c, ops)) $ Map.toList m
        deduplicate m ((g, c, ops):xs) = deduplicate (Map.insertWith (\(g1, c1) (g2, c2) -> (mergeGraphs g1 g2, c1)) ops (g, c) m) xs

        mergeGraphs :: GraphLevel2 -> GraphLevel2 -> GraphLevel2
        mergeGraphs = Map.unionWith (\n1 n2 -> if distance n1 < distance n2 then n1 else n2)

        dedupedGraphs :: [(GraphLevel2, Int, Set.Set Label)]
        dedupedGraphs = deduplicate Map.empty nextLevels

        splitGraphs :: [(GraphLevel2, Int, Set.Set Label)] -> ([(GraphLevel2, Int, Set.Set Label)], [(GraphLevel2, Int, Set.Set Label)])
        splitGraphs [] = ([], [])
        splitGraphs (g@(_, _, s):xs)
            | Set.size s == openableCount = (g:openAlls, stillToOpen)
            | otherwise = (openAlls, g:stillToOpen)
            where
                (openAlls, stillToOpen) = splitGraphs xs

        (allOpenGraphs, notAllOpenGraphs) = splitGraphs dedupedGraphs

        keepGoods :: [(GraphLevel2, Int, Set.Set Label)]
        keepGoods = if n == 0 then notAllOpenGraphs else filter (((scpl ! (n - 1)) <=) . (*(-1)) . minimumWithDefault 0 . map distance . (\(a,_,_) -> Map.elems a)) notAllOpenGraphs

scoresPerLevel :: [ParsedLine] -> Array Int Int
scoresPerLevel = listArray (0,30) . map ((*(-1)) . minimumWithDefault 0 . map dist . concat . map (\(a,_,_) -> Map.elems a)) . doRunLevel

minimumWithDefault :: Ord a => a -> [a] -> a
minimumWithDefault n [] = n
minimumWithDefault _ xs = minimum xs 

solve2 :: [ParsedLine2] -> Int
solve2 lines = (*(-1)) . minimumWithDefault 0 . map distance . concat . map (\(a,_,_) -> Map.elems a) $ runLevels2 gLevel [firstLevelInfo] 0 openableCount scpl
    where
        openableCount = length $ filter ((>0) . rate) lines
        gLevel = convertToGraphLevel2 (crossProduct lines lines) Map.empty
        firstLevel = Map.update (\(G2N n1 n2 d o) -> Just $ G2N n1 n2 0 o) ("AA", "AA") $ gLevel
        firstLevelInfo = (firstLevel, 0, Set.empty)
        scpl = scoresPerLevel lines