module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ noneOf "\""
  char '"'
  return $ String x

readExpr :: String -> String
readExpr input = case parse (spaces >> symbol) "lisp" input of
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value"

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ args !! 0
