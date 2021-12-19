module Main where

import Data.List
import Data.Function
import Data.Maybe
import Data.Char
import Data.Array
import System.IO
import Control.Applicative

type ParsedLine = Packet

data Packet
    = Literal {version :: Int, typeID :: Int, value :: Int, len :: Int}
    | Operator {version :: Int, typeID :: Int, subpackets :: [Packet], len :: Int}
    deriving (Show)

type Binary = [Int]

main :: IO ()
main = mainLoop []

mainLoop :: [ParsedLine] -> IO ()
mainLoop xs = do
    done <- isEOF
    if done
        then do putStrLn . ("Part One: " ++) . show . solve1 $ head xs
                putStrLn . ("Part Two: " ++) . show . solve2 $ head xs
        else do input <- pure convert <*> getLine
                mainLoop (xs ++ [input])

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy delim list
    | null last = [first]
    | otherwise = first : splitBy delim (tail last)
    where
        (first,last) = break (==delim) list

convert :: String -> ParsedLine
convert = parsePacket . lineToBin

lineToBin :: String -> Binary
lineToBin = concat . map hexToBin

hexToBin :: Char -> Binary
hexToBin x
    | x `elem` ['0'..'9'] = pad 4 $ decToBin (ord x - ord '0')
    | otherwise = pad 4 $ decToBin (ord x - ord 'A' + 10)

decToBin :: Int -> Binary
decToBin 0 = []
decToBin n
    | odd n = decToBin (n `div` 2) ++ [1]
    | otherwise = decToBin (n `div` 2) ++ [0]

pad :: Int -> [Int] -> [Int]
pad n xs = replicate (n - length xs) 0 ++ xs

binToDec :: Binary -> Int
binToDec = foldl (\acc x -> 2*acc + x) 0

parsePacket :: Binary -> Packet
parsePacket bin
    | typeID == 4 = Literal {version = version, typeID = typeID, value = value, len = bitsUsed + 6}
    | otherwise = Operator {version = version, typeID = typeID, subpackets = subpackets, len = 7 + lengthTypeBits + totalSubpacketLength}
    where
        (versionBin, rest) = splitAt 3 bin
        version = binToDec versionBin
        
        (typeIDBin, rest') = splitAt 3 rest
        typeID = binToDec typeIDBin
        
        (value, bitsUsed) = parseLiteral rest' 0 0

        ([lengthTypeID], rest'') = splitAt 1 rest'

        lengthTypeBits = if lengthTypeID == 0 then 15 else 11
        (lengthValueBin, rest''') = splitAt lengthTypeBits rest''
        lengthValue = binToDec lengthValueBin

        subpackets = if lengthTypeID == 0
            then parseSubPackets (-1) (take lengthValue rest''')
            else parseSubPackets lengthValue rest'''

        totalSubpacketLength = sum . map len $ subpackets


parseSubPackets :: Int -> Binary -> [Packet]
parseSubPackets 0 _ = []
parseSubPackets _ [] = []
parseSubPackets n bin = packet : parseSubPackets (n-1) (drop (len packet) bin) 
    where
        packet = parsePacket bin


parseLiteral :: Binary -> Int -> Int -> (Int, Int)
parseLiteral (b:bs) val usedBits
    | b == 0 = (val', usedBits')
    | otherwise = parseLiteral d val' usedBits'
    where
        usedBits' = usedBits + 5
        val' = val * 2^4 + binToDec t
        (t,d) = splitAt 4 bs

solve1 :: Packet -> Int
solve1 = sumVersions

sumVersions :: Packet -> Int
sumVersions Literal{version = version} = version
sumVersions Operator{version = version, subpackets = subpackets} = version + sum (map sumVersions subpackets)


evalPacket :: Packet -> Int
evalPacket Literal{value = value} = value
evalPacket Operator{typeID = 0, subpackets = subpackets} = sum . map evalPacket $ subpackets
evalPacket Operator{typeID = 1, subpackets = subpackets} = product . map evalPacket $ subpackets
evalPacket Operator{typeID = 2, subpackets = subpackets} = minimum . map evalPacket $ subpackets
evalPacket Operator{typeID = 3, subpackets = subpackets} = maximum . map evalPacket $ subpackets
evalPacket Operator{typeID = 5, subpackets = [first, second]} = fromEnum $ evalPacket first > evalPacket second
evalPacket Operator{typeID = 6, subpackets = [first, second]} = fromEnum $ evalPacket first < evalPacket second
evalPacket Operator{typeID = 7, subpackets = [first, second]} = fromEnum $ evalPacket first == evalPacket second

solve2 :: Packet -> Int
solve2 = evalPacket