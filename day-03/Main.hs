module Main where

import Data.List

type Coordinate = (Int, Int)
type Count      = Int
type Height     = Int
type Line       = [Int]
type Step       = (Int, Int)
type Width      = Int

type Stepper    = (Coordinate -> Coordinate)

data Map = Map {
    mapLines    :: [Line],
    mapWidth    :: Width,
    mapHeight   :: Height
} deriving (Show)

createMap :: [String] -> Map
createMap ss = Map (map (elemIndices '#') ss) (length $ head ss) (length ss)

mapComplete :: Map -> Coordinate -> Bool
mapComplete m cs = snd cs >= mapHeight m

step :: Width -> Step -> Coordinate -> Coordinate
step w (sx, sy) (cx, cy) = ((cx + sx) `mod` w, cy + sy)

tree :: Map -> Coordinate -> Bool
tree m (x, y)
    | mapComplete m (x, y)  = False
    | otherwise             = (not . null) $ elemIndices x line
        where line = (mapLines m !! y)

hit :: Map -> Stepper -> Coordinate -> Count -> Count
hit m stepper curr count
    | mapComplete m curr    = count
    | otherwise             = hit m stepper next updated
         where next      = stepper curr
               updated
                | tree m curr = count + 1
                | otherwise   = count

solve :: Map -> Step -> Count
solve m s = hit m stepper start 0
     where stepper = step (mapWidth m) s
           start   = (0, 0)

solve2 :: Map -> [Step] -> Count
solve2 m ss = foldr (*) 1 hs
    where hs = map (solve m) ss

main :: IO ()
main = do
    xs <- fmap lines (readFile "input/input.txt")
    print $ solve (createMap xs) (3, 1)
    print $ solve2 (createMap xs) [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
