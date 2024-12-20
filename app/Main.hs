{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Exception (throw)
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day11
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
            (3, 2) -> Day3.day3part2
            (4, 1) -> Day4.day4
            (4, 2) -> Day4.day4part2
            (5, 1) -> Day5.day5
            (5, 2) -> Day5.day5part2
            (6, 1) -> Day6.day6
            (6, 2) -> Day6.day6part2
            (7, 1) -> Day7.day7
            (7, 2) -> Day7.day7part2
            (11, 1) -> Day11.day11
            (11, 2) -> Day11.day11part2
            _ -> error "Day not implemented"
        )
        input

    return ()
