module Day3 where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

number :: Parser Integer
number = do
    s <- some digitChar
    return (read s)

mul :: Parser Integer
mul = do
    void $ string "mul("
    n1 <- number
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
    let res = case runParser p1 "" i of
            Right res -> res
            Left err -> error (errorBundlePretty err)

    print res
    print $ sum res

    return ()
