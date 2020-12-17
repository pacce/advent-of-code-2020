module Main where
import Data.Maybe (isJust)

type Value  = Int
type Window = [Int]

chunk :: Int -> [Value] -> [(Value, Window)]
chunk k ys = filter (\(_, v) -> length v == k) (ck k ys)
    where ck :: Int -> [Value] -> [(Value, Window)]
          ck k [] = []
          ck k (x:xs) = [(x, take k xs)] ++ ck k xs

eval :: (Value, Window) -> Maybe Value
eval (x, ws)
    | null ys       = Just x
    | otherwise     = Nothing
    where ys = [w1 + w2 | w1 <- ws, w2 <- ws, w1 + w2 == x, w1 /= w2]

solve :: Int -> [Value] -> Maybe Value
solve k xs = head $ filter isJust ys
    where ys = map eval (chunk k $ reverse xs)

main :: IO ()
main = do
    xs <- fmap lines $ readFile "input/input.txt"
    print $ solve 25 (fmap read xs)
