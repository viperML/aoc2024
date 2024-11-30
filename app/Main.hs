module Main where

import Day1
import Options.Applicative

data Cli = Cli
    { day :: String
    , input_file :: FilePath
    }
    deriving (Show)

cli :: Parser Cli
cli =
    Cli
        <$> strOption
            (long "day" <> short 'd')
        <*> strOption (long "input-file" <> short 'i')

main :: IO ()
main = do
    parsedCli <- execParser $ info (cli <**> helper) fullDesc
    print parsedCli

    input <- readFile parsedCli.input_file

    case day parsedCli of
        "1" -> Day1.day1 input
        _ -> putStrLn "Day not implemented"

    return ()
