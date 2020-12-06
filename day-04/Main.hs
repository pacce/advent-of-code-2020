{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import Text.Parsec.Text

type Key    = String
type Value  = String

(<:>) a b = (:) <$> a <*> b

key :: Stream s m Char => ParsecT s u m Key
key = choice
    [ string "byr"
    , string "iyr"
    , (char 'e' <:> (string "yr" <|> string "cl"))
    , (char 'h' <:> (string "gt" <|> string "cl"))
    , string "pid"
    , string "cid"
    ]

value :: Stream s m Char => ParsecT s u m Value
value = choice
    [ many1 alphaNum
    , (char '#' <:> (many1 alphaNum))
    ]

separator :: Stream s m Char => ParsecT s u m Char
separator = space <|> tab <|> newline

field :: Stream s m Char => ParsecT s u m (Key, Value)
field = do
    { k <- key
    ; _ <- char ':'
    ; v <- value
    ; _ <- separator
    ; return (k, v)
    }

passport :: Stream s m Char => ParsecT s u m [(Key, Value)]
passport = many1 field

passports :: Stream s m Char => ParsecT s u m [[(Key, Value)]]
passports = sepBy passport endOfLine

main :: IO ()
main = do
    { result <- parseFromFile passports "./input/input.txt"
    ; case result of
        Left err -> print err
        Right ps -> print $ length ps
    }
