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

type ParsedLine = String

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
convert = id

convert2 :: String -> ParsedLine2
convert2 = convert

data Dir = N | E | S | W deriving (Eq, Show)

lookAheadVecs :: Dir -> [(Int, Int)]
lookAheadVecs N = [(-1,-1), (-1,0), (-1,1)]
lookAheadVecs E = [(-1,1), (0,1), (1,1)]
lookAheadVecs S = [(1,-1), (1,0), (1,1)]
lookAheadVecs W = [(-1,-1), (0,-1), (1,-1)]

moveVecs :: Dir -> (Int, Int)
moveVecs N = (-1,0)
moveVecs E = (0,1)
moveVecs S = (1,0)
moveVecs W = (0,-1)

allNeighborVecs :: [(Int, Int)]
allNeighborVecs = [(1,1), (1,0), (1,-1), (0,1), (0,-1), (-1,1), (-1,0), (-1,-1)]

addT :: (Int, Int) -> (Int, Int) -> (Int, Int)
addT (x,y) (z,w) = (x + z, y + w)

proposeOneElf :: [Dir] -> Set.Set (Int, Int) -> (Int, Int) -> Map.Map (Int, Int) [(Int, Int)] -> Map.Map (Int, Int) [(Int, Int)]
proposeOneElf dirs elves elf proposals
    | allNeighborsEmpty = proposals
    | null proposedDir = proposals
    | otherwise = newProposals
    where
        allNeighborsEmpty = and . map (flip Set.notMember elves . addT elf) $ allNeighborVecs
        proposedDir = take 1 . map fst . filter snd . zip dirs . map (all (flip Set.notMember elves) . map (addT elf) . lookAheadVecs) $ dirs
        [singleProposedDir] = proposedDir
        proposedPos = addT elf (moveVecs singleProposedDir)
        newProposals = Map.insertWith (++) proposedPos [elf] proposals

propose :: [Dir] -> Set.Set (Int, Int) -> Map.Map (Int, Int) [(Int, Int)]
propose dirs elves = Set.foldr (proposeOneElf dirs elves) Map.empty elves

performProposal :: ((Int, Int), [(Int, Int)]) -> Set.Set (Int, Int) -> Set.Set (Int, Int)
performProposal (proposal, [elf]) elves = Set.insert proposal (Set.delete elf elves)
performProposal _ elves = elves

performProposals :: Set.Set (Int, Int) -> Map.Map (Int, Int) [(Int, Int)] -> Set.Set (Int, Int)
performProposals elves proposals = foldr performProposal elves . Map.assocs $ proposals

rotateDirs :: [Dir] -> [Dir]
rotateDirs (x:xs) = xs ++ [x]

doRounds :: Int -> [Dir] -> Set.Set (Int, Int) -> Set.Set (Int, Int)
doRounds 0 _ elves = elves
doRounds n dirs elves = doRounds (n - 1) (rotateDirs dirs) newElves
    where
        proposals = propose dirs elves
        newElves = performProposals elves proposals

doRoundsUntilNoChange :: [Dir] -> Set.Set (Int, Int) -> Int
doRoundsUntilNoChange dirs elves
    | elves == newElves = 1
    | otherwise = 1 + doRoundsUntilNoChange (rotateDirs dirs) newElves
    where
        proposals = propose dirs elves
        newElves = performProposals elves proposals

getBounds :: Set.Set (Int, Int) -> ((Int, Int), (Int, Int))
getBounds elves = ((minx, maxx), (miny, maxy))
    where
        minx = fst . minimumBy (compare `on` fst) $ elves
        miny = snd . minimumBy (compare `on` snd) $ elves
        maxx = fst . maximumBy (compare `on` fst) $ elves
        maxy = snd . maximumBy (compare `on` snd) $ elves

getBoundingBoxArea :: Set.Set (Int, Int) -> Int
getBoundingBoxArea elves = (maxx - minx + 1) * (maxy - miny + 1)
    where
        ((minx, maxx), (miny, maxy)) = getBounds elves 

scoreElves :: Set.Set (Int, Int) -> Int
scoreElves elves = getBoundingBoxArea elves - Set.size elves

elvesToList :: Set.Set (Int, Int) -> [String]
elvesToList elves = [[if Set.member (x,y) elves then '#' else '.' | y <- [miny..maxy]] | x <- [minx..maxx]]
    where
        ((minx, maxx), (miny, maxy)) = getBounds elves

showElvesList :: [String] -> IO ()
showElvesList [] = return ()
showElvesList (x:xs) = do 
    putStrLn x
    showElvesList xs

solve1 :: [ParsedLine] -> Int
solve1 lines = scoreElves elvesAfterRounds
    where
        elfIndices = concat [[(i,j) | ('#', j) <- zip line [0..]] | (line, i) <- zip lines [0..]]
        elfSet = Set.fromList elfIndices
        initialDirs = [N, S, W, E]
        elvesAfterRounds = doRounds 10 initialDirs elfSet

solve2 :: [ParsedLine2] -> Int
solve2 lines = doRoundsUntilNoChange initialDirs elfSet
    where
        elfIndices = concat [[(i,j) | ('#', j) <- zip line [0..]] | (line, i) <- zip lines [0..]]
        elfSet = Set.fromList elfIndices
        initialDirs = [N, S, W, E]