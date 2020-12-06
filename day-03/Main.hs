module Main where

import Data.List

type Coordinate = Int
type Count      = Int
type Line       = [Int]
type Step       = Int
type Width      = Int

data Map = Map {
    mapLines    :: [Line],
    mapWidth    :: Width
} deriving (Show)

createMap :: [String] -> Map
createMap ss = Map (map (elemIndices '#') ss) (length $ head ss)

step :: Width -> Step -> Coordinate -> Coordinate
step w s c = (c + s) `mod` w

hit :: Coordinate -> Line -> Count
hit y ls = (length $ elemIndices y ls)

hits :: [Line] -> (Coordinate -> Coordinate) -> Coordinate -> Count -> Count
hits [] _ _ count = count
hits (l:ls) stepper curr count = hits ls stepper next updated
    where next      = stepper curr
          updated   = count + (hit curr l)

solve :: Map -> Step -> Count
solve m s = hits (mapLines m) stepper start 0
    where stepper   = step (mapWidth m) s
          start     = 0

main :: IO ()
main = do
    xs <- fmap lines (readFile "input/input.txt")
    print $ solve (createMap xs) 3
