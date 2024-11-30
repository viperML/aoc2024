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

    case day parsedCli of
        "1" -> Day1.day1 parsedCli.input_file
        _ -> putStrLn "Day not implemented"

    return ()
