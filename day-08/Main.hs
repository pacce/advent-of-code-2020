{-# LANGUAGE FlexibleContexts #-}

module Main where

import Text.Parsec
import Text.Parsec.Text

number :: Stream s m Char => ParsecT s u m [Char]
number = many1 digit

negative :: Stream s m Char => ParsecT s u m Int
negative = do { s <- char '-'; n <- number; return $ read (s : n) }

positive :: Stream s m Char => ParsecT s u m Int
positive = do { _ <- char '+'; n <- number; return $ read n }

integer :: Stream s m Char => ParsecT s u m Int
integer = negative <|> positive

type Counter = Int

counter :: Stream s m Char => ParsecT s u m Counter
counter = integer

data Opcode = Acc Counter | Jmp Counter | Nop Counter
    deriving (Show)

acc :: Stream s m Char => ParsecT s u m Opcode
acc = do { _ <- string "acc"; _ <- spaces; c <- counter; _ <- newline; return $ Acc c }

jmp :: Stream s m Char => ParsecT s u m Opcode
jmp = do { _ <- string "jmp"; _ <- spaces; c <- counter; _ <- newline; return $ Jmp c }

nop :: Stream s m Char => ParsecT s u m Opcode
nop = do { _ <- string "nop"; _ <- spaces; c <- counter; _ <- newline; return $ Nop c }

opcode :: Stream s m Char => ParsecT s u m Opcode
opcode = choice [acc, jmp, nop]

type Accumulator    = Int
type Pointer        = Int
type Program        = [Opcode]

data Execution = Execution {
    accumulator  :: Accumulator,
    stackPointer :: Pointer,
    instructions :: [Pointer],
    program      :: Program
} deriving (Show)

mkExecution :: Program -> Execution
mkExecution os = Execution 0 0 [] os

stepAccumulator :: Accumulator -> Opcode -> Accumulator
stepAccumulator a (Acc i) = a + i
stepAccumulator a (Jmp _) = a
stepAccumulator a (Nop _) = a

stepPointer :: Pointer -> Opcode -> Pointer
stepPointer s (Acc _) = s + 1
stepPointer s (Jmp i) = s + i
stepPointer s (Nop _) = s + 1

stepExecution :: Execution -> Execution
stepExecution e = Execution sa sp is (program e)
    where op = program e !! stackPointer e
          sa = stepAccumulator (accumulator e) op
          sp = stepPointer (stackPointer e) op
          is = (instructions e) ++ [stackPointer e]

stopExecution :: Execution -> Bool
stopExecution e = (stackPointer e) `elem` (instructions e)

execute :: Execution -> Accumulator
execute e
    | stopExecution e   = accumulator e
    | otherwise         = execute (stepExecution e)

main :: IO ()
main = do
    { result <- parseFromFile (many1 opcode) "input/reference.txt"
    ; case result of
        Left err -> print err
        Right os -> print $ (execute . mkExecution) os
    }
