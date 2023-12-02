module Main where

import Day01 qualified

import System.Directory
import Data.Foldable


days :: [(String, IO ())]
days = [("01", Day01.main)]

main :: IO ()
main = do
    for_ days $ \(day, main') -> do
        putStrLn $ "Day " <> day
        setCurrentDirectory $ "day" <> day
        main'
        setCurrentDirectory "../"
