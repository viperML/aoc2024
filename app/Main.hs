{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Exception (throw)
import Day1
import Day2
import Day3 (day3)
import Options.Applicative

data Cli = Cli
    { day :: Integer
    , input_file :: FilePath
    , part :: Integer
    }
    deriving (Show)

cli :: Parser Cli
cli =
    Cli
        <$> option
            auto
            (long "day" <> short 'd')
        <*> strOption (long "input-file" <> short 'i')
        <*> option auto (long "part" <> short 'p')

main :: IO ()
main = do
    parsedCli <- execParser $ info (cli <**> helper) fullDesc
    print parsedCli

    input <- readFile parsedCli.input_file

    ( case (parsedCli.day, parsedCli.part) of
            (1, 1) -> Day1.day1
            (1, 2) -> Day1.day1part2
            (2, 1) -> Day2.day2
            (2, 2) -> Day2.day2part2
            (3, 1) -> Day3.day3
            _ -> error "Day not implemented"
        )
        input

    return ()
