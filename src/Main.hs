module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment (getArgs)
import Control.Monad (liftM)
import Numeric (readHex)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
               deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escapedChars <|> noneOf "\"\\"
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
             "#t" -> Bool True
             "#f" -> Bool False
             _    -> Atom atom

digits :: Parser String
digits = many1 digit

base :: Parser Char
base = char '#' >> oneOf "boxd"

-- Gets the value from a read function such as readHex
-- Since the parser handles parsing we know it will always
-- produce a value if the parser successes
getValue :: [(a, String)] -> a
getValue [(x,_)] = x
getValue _       = error "Should not happen"

hexToNum :: (Num a, Eq a) => String -> a
hexToNum = getValue . readHex

parseNumberBase :: Parser LispVal
parseNumberBase = do
  b <- base
  case b of
    'd' -> parseNumber
    'x' -> parseNumberHex

hexDigits :: Parser String
hexDigits = many1 hexDigit

parseNumberHex :: Parser LispVal
parseNumberHex = liftM (Number . hexToNum) hexDigits

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) digits

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val  -> "Found value: " ++ show val

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
