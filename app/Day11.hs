module Day11 (day11, day11part2) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer (decimal)
import Util (parseOrThrow)
import Text.Show.Pretty (pPrint)
import Data.Function.Memoize (Memoizable(memoize))

type Parser = Parsec Void String

p :: Parser [Int]
p = try $ decimal `sepEndBy` space1

digits :: (Integral a) => a -> [a]
digits 0 = [0]
digits n = reverse (go n)
  where
    go 0 = []
    go x = (x `mod` 10) : go (x `div` 10)

fromDigits :: [Int] -> Int
fromDigits = foldl (\acc x -> acc * 10 + x) 0

blink1 :: Int -> [Int]
blink1 0 = [1]
blink1 x
    | odd (length d) = [x * 2024]
    | otherwise = [firstHalf, secondHalf]
  where
    d = digits x
    secondHalf = fromDigits $ drop (length d `div` 2) d
    firstHalf = fromDigits $ take (length d `div` 2) d

blink :: [Int] -> [Int]
blink = (>>= memoize blink1)

applyN :: Int -> (a -> a) -> a -> a
applyN 1 f x = f x
applyN n f x = applyN (n - 1) f (f x)

day11 :: String -> IO ()
day11 input = do
    let parsed = parseOrThrow p input
    let res = applyN 25 blink parsed
    pPrint $ length res

day11part2 :: String -> IO ()
day11part2 input = do
    let parsed = parseOrThrow p input
    let res = applyN 75 blink parsed
    pPrint $ length res
