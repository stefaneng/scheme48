module Main where

main :: IO ()
main = do
  putStrLn "Please enter your name: "
  getLine >>= putStrLn . (++) "Hello "
