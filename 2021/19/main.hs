module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative
import qualified Data.Set as S

type ParsedLine = String

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
convert = id

type Scanner = S.Set Pos

parseScanners :: [String] -> [Scanner]
parseScanners lines = scanners
    where
        scannerInputs = map tail . splitBy [] $ lines
        scanners = map (S.fromList . map ((\[x,y,z] -> (x,y,z)) . map read . splitBy ',')) scannerInputs

solve1 :: [ParsedLine] -> Int
solve1 input = S.size . fst $ reduceListUntilEmpty scanner scanners []
    where
        (scanner:scanners) = parseScanners input 

rotateX :: Pos -> Pos
rotateX (x,y,z) = (x,-z,y)
rotateY :: Pos -> Pos
rotateY (x,y,z) = (z,y,-x)
rotateZ :: Pos -> Pos
rotateZ (x,y,z) = (-y,x,z)

fourRotates :: (Pos -> Pos) -> [Pos -> Pos]
fourRotates rot = take 4 $ iterate (.rot) id

-- Source: https://stackoverflow.com/questions/33190042/how-to-calculate-all-24-rotations-of-3d-array
allRotationFuns :: [Pos -> Pos]
allRotationFuns = 
    fourRotates rotateX ++
    map (.(rotateY . rotateY)) (fourRotates rotateX) ++
    map (.(rotateY)) (fourRotates rotateZ) ++ 
    map (.(rotateY.rotateY.rotateY)) (fourRotates rotateZ) ++
    map (.(rotateZ)) (fourRotates rotateY) ++
    map (.(rotateZ.rotateZ.rotateZ)) (fourRotates rotateY)

reduceListUntilEmpty :: Scanner -> [Scanner] -> [Pos] -> (Scanner, [Pos])
reduceListUntilEmpty result [] poss = (result, poss)
reduceListUntilEmpty result xs poss = reduceListUntilEmpty acc xxs (pos ++ poss)
    where
        (acc, xxs, pos) = reduceList result xs []

reduceList :: Scanner -> [Scanner] -> [Pos] -> (Scanner, [Scanner], [Pos])
reduceList accumulated [] poss = (accumulated, [], poss)
reduceList accumulated (scanner:scanners) poss
    | isNothing matchURScanner = (acc, scanner:scans, newPoss)
    | otherwise = reduceList newAcc scanners (pos:poss)
    where
        matchURScanner = matchUnrotatedToPts accumulated scanner
        (pos, newAcc) = fromJust matchURScanner 
        (acc, scans, newPoss) = reduceList accumulated scanners poss


matchUnrotatedToPts :: Scanner -> Scanner -> Maybe (Pos, Scanner)
matchUnrotatedToPts points scanner = matchScannersToPts points allRotations
    where
        allRotations = map (\f -> S.map f scanner) allRotationFuns

matchScannersToPts :: Scanner -> [Scanner] -> Maybe (Pos, Scanner)
matchScannersToPts _ [] = Nothing
matchScannersToPts points (scanner:scanners)
    | isMatch = Just $ (pos, S.union points matchedScanner)
    | otherwise = matchScannersToPts points scanners
    where
        (pos, matchedScanner) = matchRotatedToPts points scanner
        isMatch = matchedScanner /= S.empty

matchRotatedToPts :: Scanner -> Scanner -> (Pos, Scanner)
matchRotatedToPts points scanner = S.foldr helper ((0,0,0), S.empty) scanner
    where
        helper :: Pos -> (Pos, Scanner) -> (Pos, Scanner)
        helper pt acc
            | mbp == S.empty = acc
            | otherwise = (pos, mbp)
            where 
                (pos, mbp) = matchByPt pt points scanner

matchByPt :: Pos -> Scanner -> Scanner -> (Pos, Scanner)
matchByPt pt points scanner = S.foldr (getMatchedScanner pt scanner points) ((0,0,0), S.empty) points

getMatchedScanner :: Pos -> Scanner -> Scanner -> Pos -> (Pos, Scanner) -> (Pos, Scanner)
getMatchedScanner relative scanner points normal acc
    | S.size (S.intersection transformedScanner points) >= 12 = (pos, transformedScanner)
    | otherwise = acc
    where
        (pos, transformedScanner) = transformScanner normal relative scanner

transformScanner :: Pos -> Pos -> Scanner -> (Pos, Scanner)
transformScanner normal@(x1,y1,z1) relative@(x2,y2,z2) scanner = (diff, S.map (\(x,y,z) -> (x + d1, y + d2, z + d3)) scanner)
    where
        diff@(d1,d2,d3) = (x1 - x2, y1 - y2, z1 - z2)

type Pos = (Int, Int, Int)

choose :: Int -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose k (x:xs) = [x:c | c <- choose (k-1) xs] ++ choose k xs

solve2 :: [ParsedLine] -> Int
solve2 input = maximum dists
    where
        dists = map (\[(x1,y1,z1),(x2,y2,z2)] -> abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) posPairs
        posPairs = choose 2 poss
        poss = snd $ reduceListUntilEmpty scanner scanners []
        (scanner:scanners) = parseScanners input 