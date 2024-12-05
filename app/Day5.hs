module Day5 (day5) where

import Control.Monad (void)
import Data.Bifunctor
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Show.Pretty (pPrint)

type Rule = (Int, Int)

checkRule' :: Rule -> [Int] -> Bool
checkRule' rule = mainPass
  where
    mainPass [] = False
    mainPass (x : xs) =
        if x == fst rule
            then (snd rule `elem` xs) || mainPass xs
            else mainPass xs

checkRule :: Rule -> [Int] -> Bool
checkRule rule list = not $ checkRule' rule (reverse list)

type Parser = Parsec Void String

parseRule :: Parser Rule
parseRule = do
    n1 <- decimal
    void $ string "|"
    n2 <- decimal
    return (n1, n2)

parseList :: Parser [Int]
parseList = do
    x <- decimal
    xs <- many $ string "," *> decimal
    return (x : xs)

myParse :: [String] -> ([Rule], [[Int]])
myParse [] = ([], [])
myParse (s : ss) = case (rule, list) of
    (Right r, _) -> first (r :) next
    (_, Right l) -> second (l :) next
    _ -> next
  where
    rule = runParser parseRule "" s
    list = runParser parseList "" s
    next = myParse ss

day5 :: String -> IO ()
day5 input = do
    let (rules, updates) = myParse (lines input)
    let res = do
            update <- updates
            if all (`checkRule` update) rules
                then return $ update !! (length update `div` 2)
                else []
    print (sum res)
