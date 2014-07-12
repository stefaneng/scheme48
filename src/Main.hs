module Main where

import System.Environment (getArgs)

argumentMessage :: [String] -> Int
argumentMessage args = case length args of
                         0 -> 0
                         1 -> read (args !! 0)
                         _ -> read (args !! 0) + read (args !! 1)

main :: IO ()
main = do
  args <- getArgs
  putStr "Hello "
  putStrLn $ (show . argumentMessage) args
