{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Day7 (day7, day7part2) where

import Control.Monad
import Data.Functor ((<&>))
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Show.Pretty

type Parser = Parsec Void String

parseLine :: Parser (Int, [Int])
parseLine = do
    x1 <- decimal
    void (string ":")
    xs <- some $ space *> decimal
    return (x1, xs)

run :: Int -> Int -> [Int] -> Bool
run target acc [x] = (acc + x) == target || (acc * x) == target
run target acc (x : xs) = run target accA xs || run target accB xs
  where
    accA = acc + x
    accB = acc * x

day7 :: String -> IO ()
day7 input = do
    let l = case traverse (runParser parseLine "") (lines input) of
            Right x -> x
            Left b -> error (errorBundlePretty b)
    pPrint l

    let res :: [Int] = do
            (target, line) <- l
            if run target 0 line
                then return target
                else mempty
    pPrint res
    pPrint $ sum res

day7part2 :: String -> IO ()
day7part2 = do
    undefined
