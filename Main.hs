module Main where
import System.Environment

main = do
    putStrLn "name?"
    name <- getLine
    putStrLn $ "hello " ++ name ++ "!"
