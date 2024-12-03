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
    void $ string ","
    n2 <- number
    void $ string ")"
    s <- get
    if s
        then return (n1 * n2)
        else return 0

doParser :: Parser ()
doParser = (string "do()" >> put True) <|> (string "don't()" >> put False)

p1 :: Parser [Integer]
p1 = some $ try (skipManyTill anySingle (try mul))

-- p2 :: Parser Integer
-- p2 = skipManyTill (try (skipManyTill anySingle (doParser <|> dontParser))) (try mul)

p2 :: Parser [Integer]
p2 = do
    p <- (Left <$> doParser) <|> (Right <$> mul)
    return []


day3 :: String -> IO ()
day3 input = do
    let i = T.pack input
    print i

    res <- case runState (runParserT p2 "" i) True of
        (Right r, s) -> print s >> return r
        (Left err, s) -> error (errorBundlePretty err)

    print res
    -- print $ sum res

    return ()
