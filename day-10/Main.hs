module Main where
import qualified Data.List

chunk :: [Int] -> [(Int, Int)]
chunk []        = []
chunk (x:[])    = []
chunk (x:xs)    = [(x, head xs )] ++ chunk xs

eval :: [Int] -> (Int, Int)
eval xs = (length $ filter d1 ys, length $ filter d3 ys)
     where ys = chunk xs
           d1 = diff 1
           d3 = diff 3

diff :: Int -> (Int, Int) -> Bool
diff v vv = snd vv - fst vv == v

solve :: [Int] -> Int
solve xs
    | head ys == 1  = (1 + fst es) * (1 + snd es)
    | head ys == 3  = fst es * (2 + snd es)
    | otherwise     = fst es * (1 + snd es)
    where ys = Data.List.sort xs
          es = eval ys

main :: IO ()
main = do
    xs <- fmap lines $ readFile "input/input.txt"
    let ys = fmap (read::String -> Int) xs
    print $ solve ys
