module Util where

import Debug.Trace (traceShow)

import Data.Array.IArray as A
import Linear (V2 (V2))
import Text.Megaparsec

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x

type Map a = A.Array (V2 Int) a

parseMap :: (Char -> a) -> String -> Map a
parseMap mapping = fmap mapping . parseMapChar

parseMapChar :: String -> Map Char
parseMapChar s = A.array (V2 0 0, V2 maxlines maxchars) $ do
    (iline, line) <- zip [0 ..] (lines s)
    (ielem, c) <- zip [0 ..] line

    return (V2 iline ielem, c)
  where
    maxlines = length (lines s) - 1
    maxchars = length (head $ lines s) - 1

parseOrThrow ::
    (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> s -> a
parseOrThrow p i = case runParser p "" i of
    Right res -> res
    Left err -> error (errorBundlePretty err)
