module Main where

import System.Environment (getArgs)

argumentMessage :: [String] -> String
argumentMessage args = ", " ++ case length args of
                                 0 -> ""
                                 1 -> args !! 0
                                 2 -> args !! 0 ++
                                      ". Your second argument is: " ++
                                      args !! 1
                                 _ -> "Too many arguments!"

main :: IO ()
main = do
  args <- getArgs
  putStr "Hello"
  putStrLn $ argumentMessage args
