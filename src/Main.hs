module Main where

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStr "Hello"
  if length args > 0
  then putStrLn (", " ++ args !! 0)
  else putStrLn ""
