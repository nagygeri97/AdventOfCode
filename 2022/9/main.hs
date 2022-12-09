module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative
import Control.Monad.State
import Control.Monad
import qualified Data.Set as Set

data Direction = U | R | D | L deriving (Eq, Show)
type Move = (Direction, Int) 

type ParsedLine = Move

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
convert line
    | direction == "U" = (U, amount)
    | direction == "R" = (R, amount)
    | direction == "D" = (D, amount)
    | direction == "L" = (L, amount)
    where
        [direction, amountStr] = splitBy ' ' line
        amount = read amountStr

type Position = (Int, Int)
type RopeState = ([Position], Set.Set Position)

initialState :: Int -> RopeState
initialState n = (replicate n (0,0), Set.singleton (0,0))

performMoves :: [Move] -> State RopeState Int
performMoves [] = countVisited
performMoves (m:ms) = do
    newMove <- performMove m
    if isNothing newMove
        then performMoves ms
        else performMoves ((fromJust newMove):ms)

performMove :: Move -> State RopeState (Maybe Move)
performMove (dir, amt) = do
    (rope, ps) <- get
    let newRope = moveAll rope dir
    let newPs = Set.insert (last newRope) ps
    put (newRope, newPs)
    if amt > 1
        then return (Just (dir, amt - 1))
        else return Nothing

moveHead :: Direction -> Position -> Position
moveHead U (hx, hy) = (hx + 1, hy)
moveHead R (hx, hy) = (hx, hy + 1)
moveHead D (hx, hy) = (hx - 1, hy)
moveHead L (hx, hy) = (hx, hy - 1)

moveAll :: [Position] -> Direction -> [Position]
moveAll (h:rope) dir = newH : moveTails newH rope 
    where
        newH = moveHead dir h

moveTails :: Position -> [Position] -> [Position]
moveTails _ [] = []
moveTails current (t:ts) = newT : moveTails newT ts
    where
        newT = moveTail current t

moveTail :: Position -> Position -> Position
moveTail (hx, hy) t@(tx, ty)
    | dx >= 2 || dy >= 2 = (tx + sx, ty + sy)
    | otherwise = t
    where
        dx = abs (hx - tx)
        dy = abs (hy - ty)
        sx = signum (hx - tx)
        sy = signum (hy - ty)

countVisited :: State RopeState Int
countVisited = do
    (_, ps) <- get
    return $ Set.size ps

evalLastMovesWithLength :: Int -> [ParsedLine] -> Int
evalLastMovesWithLength n lines = evalState (performMoves lines) (initialState n)

convert2 :: String -> ParsedLine2
convert2 = convert

solve1 :: [ParsedLine] -> Int
solve1 = evalLastMovesWithLength 2

solve2 :: [ParsedLine2] -> Int
solve2 = evalLastMovesWithLength 10