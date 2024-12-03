module Day3 where

import Control.Monad
import Control.Monad.State
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Parser = ParsecT Void T.Text (State Bool)

number :: Parser Integer
number = read <$> some digitChar

mul :: Parser Integer
mul = do
    void $ string "mul("
    n1 <- number
    void $ string ","
    n2 <- number
    void $ string ")"
    s <- get
    if s
        then return (n1 * n2)
        else empty

doParser :: Parser ()
doParser = (string "do()" >> put True) <|> (string "don't()" >> put False)

p1 :: Parser [Integer]
p1 = many $ try (skipManyTill anySingle (try mul))

p2 :: Parser [Integer]
p2 = many $ try (skipManyTill (doParser <|> void anySingle) (try mul))

dayFor :: Parser [Integer] -> String -> IO ()
dayFor p input = do
    res <- case runState (runParserT p "" (T.pack input)) True of
        (Right r, s) -> print s >> return r
        (Left err, s) -> error (errorBundlePretty err)

    print res
    print $ sum res

day3 :: String -> IO ()
day3 = dayFor p1

day3part2 :: String -> IO ()
day3part2 = dayFor p2
