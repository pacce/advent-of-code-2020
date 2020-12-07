module Main where

type BSP    = [Char]
type Range  = (Int, Int)

type Column = Int
type Id     = Int
type Row    = Int
type Seat   = (Row, Column)

data Partition
    = Lower
    | Upper
    deriving (Show)

amplitude :: Range -> Int
amplitude (x, y) = (y - x) `div` 2

parse :: Char -> Partition
parse c
    | c == 'F' = Lower
    | c == 'B' = Upper
    | c == 'L' = Lower
    | c == 'R' = Upper

bsp :: BSP -> [Partition]
bsp = map parse

partition :: Partition -> Range -> Range
partition Lower (x, y) = (x, x + amplitude (x, y))
partition Upper (x, y) = (x + amplitude (x, y) + 1, y)

horizontal :: Range -> [Partition] -> Row
horizontal r []     = fst r
horizontal r (p:ps) = horizontal (partition p r) ps

vertical :: Range -> [Partition] -> Column
vertical r []     = fst r
vertical r (p:ps) = vertical (partition p r) ps

seat :: [Partition] -> Seat
seat ps = (horizontal (0, 127) rs, vertical (0, 7) cs)
    where (rs, cs) = splitAt 7 ps

uid :: Seat -> Id
uid (x, y) = x * 8 + y

solve :: [BSP] -> [Id]
solve = map (uid . seat . bsp)

main :: IO ()
main = do
    xs <- fmap lines (readFile "input/reference.txt")
    print $ maximum (solve xs)
