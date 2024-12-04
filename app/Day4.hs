{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day4 (day4, day4part2) where

import qualified Control.Arrow as A
import Data.Array.IArray ((!))
import Data.Array.IArray as A
import Data.Functor ((<&>))
import Data.List (find, transpose)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

findXmas :: Parser Int
findXmas = many (try $ skipManyTill anySingle (try $ string "XMAS")) <&> length

findXmas' :: String -> Int
findXmas' s = case runParser findXmas "" s of
    Right c -> c
    Left e -> error (errorBundlePretty e)

diagonals :: [[a]] -> [[a]]
diagonals = tail . go []
  where
    go b es_ =
        [h | h : _ <- b] : case es_ of
            [] -> transpose ts
            e : es -> go (e : ts) es
      where
        ts = [t | _ : t <- b]

day4 :: String -> IO ()
day4 input = do
    let components =
            sum . fmap findXmas'
                <$> [ lines input -- horizontal
                    , reverse <$> lines input -- horizontal rev
                    , transpose (lines input) -- vertical
                    , reverse <$> transpose (lines input) -- vertical rev
                    , diagonals (lines input) -- diagonal
                    , reverse <$> diagonals (lines input) -- diagonal rev
                    , diagonals (reverse <$> lines input) -- other diagonal
                    , reverse <$> diagonals (reverse <$> lines input) -- other diagonal rev
                    ]

    mapM_ print components
    print (sum components)

type Coord = (Int, Int)
type Matrix a = A.Array Coord a

{-
 a . c
 . A .
 d . b
-}

isCross :: Char -> Char -> Char -> Char -> _
isCross a b c d = all (`elem` ['M', 'S']) [a, b, c, d] && (a /= b) && (c /= d)

day4part2 :: String -> IO ()
day4part2 input = do
    -- Load into Array
    let y :: [(Coord, Char)] = do
            (i, others) <- zip [0 ..] (zip [0 ..] <$> lines input)
            (j, c) <- others
            return ((i, j), c)
    let n = length (lines input)
    let m = length (head $ lines input)
    let ainput :: Matrix Char = A.array ((0, 0), (n - 1, m - 1)) y

    let newArr = do
            i <- [1 .. n - 2]
            j <- [1 .. m - 2]
            let center = ainput ! (i, j)

            let a = ainput ! (i - 1, j - 1)
            let b = ainput ! (i + 1, j + 1)
            let c = ainput ! (i - 1, j + 1)
            let d = ainput ! (i + 1, j - 1)

            return ((center == 'A') && isCross a b c d)

    print $ length (filter id newArr)
