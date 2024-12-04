{-# LANGUAGE PartialTypeSignatures #-}

module Day4 (day4) where

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
