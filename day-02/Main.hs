{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import Text.Parsec.Text

data Policy = Policy {
    low         :: Int,
    high        :: Int,
    key         :: Char,
    password    :: [Char]
} deriving (Show)

policy :: Stream s m Char => ParsecT s u m Policy
policy = do
   { l  <- many1 digit
   ; _  <- char '-'
   ; h  <- many1 digit
   ; _  <- spaces
   ; k  <- letter
   ; _  <- char ':'
   ; _  <- spaces
   ; ss <- many1 letter
   ; _  <- newline
   ; return (Policy (read l) (read h) k ss)
   }

policies :: Stream s m Char => ParsecT s u m [Policy]
policies = many1 policy

valid :: Policy -> Bool
valid p = (low p) <= count && count <= (high p)
    where count = length $ filter (\c -> key p == c) (password p)

main :: IO ()
main = do
    { result <- parseFromFile policies "./input/input.txt"
    ; case result of
        Left err -> print err
        Right ps -> print ps
    }

