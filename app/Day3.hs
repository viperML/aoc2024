module Day3 where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = ParsecT Void T.Text (State Bool)

number :: Parser Integer
number = do
    s <- some digitChar
    return (read s)

mul :: Parser Integer
mul = do
    void $ string "mul("
    n1 <- number
    put False
    void $ string ","
    n2 <- number
    void $ string ")"
    return (n1 * n2)

p1 :: Parser [Integer]
p1 = some $ try (skipManyTill anySingle (try mul))

day3 :: String -> IO ()
day3 input = do
    let i = T.pack input
    print i

    let res = case runState (runParserT p1 "" i) False of
            (Right r, _) -> r
            (Left err, _) -> error (errorBundlePretty err)

    print res
    print $ sum res

    return ()
