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

data Tile = Open | Wall | Invalid deriving (Eq, Show)
data Instruction = Move Int | TurnR | TurnL deriving (Eq, Show)
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

padTo :: Int -> a -> [a] -> [a]
padTo n e xs = xs ++ replicate (n - length xs) e

toTile :: Char -> Tile
toTile ' ' = Invalid
toTile '.' = Open
toTile '#' = Wall

indexOf :: Eq a => a -> [a] -> Int
indexOf e (x:xs)
    | e == x = 0
    | otherwise = 1 + indexOf e xs

parseInstructions :: String -> [Instruction]
parseInstructions [] = []
parseInstructions ('L':xs) = TurnL : parseInstructions xs
parseInstructions ('R':xs) = TurnR : parseInstructions xs
parseInstructions xs = (Move (read amt)) : parseInstructions rest
    where
        (amt, rest) = span isDigit xs

data Facing = R | D | L | U deriving (Eq, Show) 

turnR :: Facing -> Facing
turnR R = D
turnR D = L
turnR L = U
turnR U = R

turnL :: Facing -> Facing
turnL R = U
turnL D = R
turnL L = D
turnL U = L

facingVector :: Facing -> (Int, Int)
facingVector R = (0,1)
facingVector D = (1,0)
facingVector L = (0,-1)
facingVector U = (-1,0)

getNextCoord :: Array (Int, Int) Tile -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int)
getNextCoord field (x,y) (cx, cy) (dx,dy)
    | nTile == Open = (nx, ny)
    | nTile == Wall = (x, y)
    | nTile == Invalid = getNextCoord field (x, y) (nx, ny) (dx, dy)
    where
        ((minx, miny), (maxx, maxy)) = bounds field
        (nx, ny) = ((cx + dx) `mod` (maxx + 1), (cy + dy) `mod` (maxy + 1))
        nTile = field ! (nx, ny)


performMove :: Array (Int, Int) Tile -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int)
performMove field (x, y) (dx, dy) n
    | n == 0 = (x, y)
    | (x,y) == (nx, ny) = (x,y)
    | otherwise = performMove field (nx, ny) (dx, dy) (n - 1)
    where
        (nx, ny) = getNextCoord field (x,y) (x,y) (dx,dy)

simulateInstruction :: Instruction -> Array (Int, Int) Tile -> (Int, Int, Facing) -> (Int, Int, Facing)
simulateInstruction TurnR _ (x, y, f) = (x, y, turnR f)
simulateInstruction TurnL _ (x, y, f) = (x, y, turnL f)
simulateInstruction (Move n) field (x, y, f) = (nx, ny, f)
    where
        (nx, ny) = performMove field (x,y) (facingVector f) n


simulateInstructions :: [Instruction] -> Array (Int, Int) Tile -> (Int, Int, Facing) -> (Int, Int, Facing)
simulateInstructions [] _ pos = pos
simulateInstructions (i:is) field pos = simulateInstructions is field (simulateInstruction i field pos)

scoreFacing :: Facing -> Int
scoreFacing R = 0
scoreFacing D = 1
scoreFacing L = 2
scoreFacing U = 3

scorePos :: (Int, Int, Facing) -> Int
scorePos (x, y, f) = 1000 * (x + 1) + 4 * (y + 1) + scoreFacing f

solve1 :: [ParsedLine] -> Int
solve1 lines = scorePos finalPos
    where
        [field, [instrStr]] = splitBy [] lines
        w = maximum . map length $ field
        normField = map (padTo w ' ') field
        fieldArray = listArray ((0,0), ((length field - 1, w - 1))) . map toTile . concat $ normField
        instructions = parseInstructions instrStr
        initialPos = (0, indexOf '.' (normField !! 0), R)

        finalPos = simulateInstructions instructions fieldArray initialPos

cubeRules :: Int -> Facing -> (Int, Facing)
-- cubeRules 2 R = (11, L)
-- cubeRules 11 R = (2, L)
-- cubeRules 2 U = (4, D)
-- cubeRules 4 U = (2, D)
-- cubeRules 2 L = (5, D)
-- cubeRules 5 U = (2, R)
-- cubeRules 4 L = (11, U)
-- cubeRules 11 D = (4, R)
-- cubeRules 4 D = (10, U)
-- cubeRules 10 D = (4, U)
-- cubeRules 5 D = (10, R)
-- cubeRules 10 L = (5, U)
-- cubeRules 6 R = (11, D)
-- cubeRules 11 U = (6, L)
-- cubeRules c f = error (show c ++ " " ++ show f)
cubeRules 1 L = (6, R)
cubeRules 6 L = (1, R)
cubeRules 1 U = (9, R)
cubeRules 9 L = (1, D)
cubeRules 2 U = (9, U)
cubeRules 9 D = (2, D)
cubeRules 2 R = (7, L)
cubeRules 7 R = (2, L)
cubeRules 2 D = (4, L)
cubeRules 4 R = (2, U)
cubeRules 4 L = (6, D)
cubeRules 6 U = (4, R)
cubeRules 7 D = (9, L)
cubeRules 9 R = (7, U)

addIndices :: Int -> [[Tile]] -> [[(Tile, Int)]]
addIndices _ [] = []
addIndices row (x:xs) = (zip x . map (+((row `div` gridSize) * rowSize)) . concat $ [replicate gridSize i | i<-[0..]]) : addIndices (row + 1) xs

gridSize :: Int
-- gridSize = 4
gridSize = 50

rowSize :: Int
-- rowSize = 4
rowSize = 3

relativeIndex :: (Int, Int) -> (Int, Int)
relativeIndex (x, y) = (x `mod` gridSize, y `mod` gridSize)

cubeIdxFromCoords :: (Int, Int) -> Int
cubeIdxFromCoords (x, y) = (x `div` gridSize) * rowSize + (y `div` gridSize)

newRelativeIdx :: (Int, Int) -> Facing -> Facing -> (Int, Int)
newRelativeIdx (rx, ry) oldF newF
    | (oldF == D && newF == D) || (oldF == U && newF == U) = ((gridSize - 1) - rx, ry)
    | (oldF == R && newF == R) || (oldF == L && newF == L) = (rx, (gridSize - 1) - ry)
    | (oldF == R && newF == L) || (oldF == L && newF == R) = ((gridSize - 1) - rx, ry)
    | (oldF == U && newF == D) || (oldF == D && newF == U) = (rx, (gridSize - 1) - ry)
    | (oldF == R && newF == D) || (oldF == L && newF == U) = ((gridSize - 1) - ry, (gridSize - 1) - rx)
    | (oldF == R && newF == U) || (oldF == L && newF == D) = (ry, rx)
    | (oldF == U && newF == L) || (oldF == D && newF == R) = ((gridSize - 1) - ry, (gridSize - 1) - rx)
    | (oldF == U && newF == R) || (oldF == D && newF == L) = (ry, rx)

absoluteIdx :: (Int, Int) -> Int -> (Int, Int)
absoluteIdx (rx, ry) cid = ((cid `div` rowSize) * gridSize + rx, (cid `mod` rowSize) * gridSize + ry)

getNextCoord2 :: Array (Int, Int) Tile -> (Int, Int, Facing) -> (Int, Int, Facing)
getNextCoord2 field (x,y,f)
    | nx > maxx || nx < minx || ny > maxy || ny < miny || nTile == Invalid = invResult
    | nTile == Open = (nx, ny, f)
    | nTile == Wall = (x, y, f)
    where
        (dx,dy) = facingVector f
        ((minx, miny), (maxx, maxy)) = bounds field
        (nx, ny) = (x + dx, y + dy)
        nTile = field ! (nx, ny)

        cubeIdx = cubeIdxFromCoords (x, y)
        (newCube, newFacing) = cubeRules cubeIdx f
        (nrx, nry) = newRelativeIdx (relativeIndex (x, y)) f newFacing
        (nax, nay) = absoluteIdx (nrx, nry) newCube
        nnTile = field ! (nax, nay)
        invResult = case nnTile of
            Open -> (nax, nay, newFacing)
            Wall -> (x, y, f)

performMove2 :: Array (Int, Int) Tile -> (Int, Int, Facing) -> Int -> (Int, Int, Facing)
performMove2 field (x, y, f) n
    | n == 0 = (x, y, f)
    | (x, y, f) == (nx, ny, nf) = (x, y, f)
    | otherwise = performMove2 field (nx, ny, nf) (n - 1)
    where
        (nx, ny, nf) = getNextCoord2 field (x, y, f)

simulateInstruction2 :: Instruction -> Array (Int, Int) Tile -> (Int, Int, Facing) -> (Int, Int, Facing)
simulateInstruction2 TurnR _ (x, y, f) = (x, y, turnR f)
simulateInstruction2 TurnL _ (x, y, f) = (x, y, turnL f)
simulateInstruction2 (Move n) field (x, y, f) = (nx, ny, nf)
    where
        (nx, ny, nf) = performMove2 field (x, y, f) n


simulateInstructions2 :: [Instruction] -> Array (Int, Int) Tile -> (Int, Int, Facing) -> (Int, Int, Facing)
simulateInstructions2 [] _ pos = pos
simulateInstructions2 (i:is) field pos = simulateInstructions2 is field (simulateInstruction2 i field pos)

solve2 :: [ParsedLine2] -> Int
solve2 lines = scorePos finalPos
    where
        [field, [instrStr]] = splitBy [] lines
        w = maximum . map length $ field
        normField = map (padTo w ' ') field
        fieldArray = listArray ((0,0), ((length field - 1, w - 1))) . map toTile . concat $ normField
        instructions = parseInstructions instrStr
        initialPos = (0, indexOf '.' (normField !! 0), R)

        finalPos = simulateInstructions2 instructions fieldArray initialPos