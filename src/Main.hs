module Main where

import Control.Monad (liftM)
import Numeric (readHex, readInt, readOct, readFloat)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative ((<$>), (<*>))
import Data.Ratio
import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | String String
             | Bool Bool
             | Character Char
             | Real Double
             | Rational Rational
             | Complex (Complex Double)
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
  return $ Atom atom

parseHash :: Parser LispVal
parseHash = do
  c <- hashchar
  case c of
    -- Will always be one of these values since
    -- hashchar parses only these characters
    't' -> return $ Bool True
    'f' -> return $ Bool False
    'd' -> parseNumber
    'x' -> parseNumberHex
    'o' -> parseNumberOct
    'b' -> parseNumberBin
    '\\' -> parseChar

parseChar :: Parser LispVal
parseChar = do
  c <- anyChar
  return $ Character c

getDouble :: String -> Double
getDouble = getValue . readFloat

toDouble :: LispVal -> Double
toDouble (Real f)  = f
toDouble (Integer n) = fromIntegral n

parseReal :: Parser LispVal
parseReal = liftM (Real . getDouble) float
    where float = (++) <$> digits <*> decimal
          decimal = (:) <$> char '.' <*> digits

parseRational :: Parser LispVal
parseRational = do
  numer <- digits
  char '/'
  denom <- digits
  return $ Rational ((read numer) % (read denom))

parseComplex :: Parser LispVal
parseComplex = do
  x <- (try parseReal <|> parseInteger)
  char '+'
  y <- (try parseReal <|> parseInteger)
  char 'i'
  return $ Complex (toDouble x :+ toDouble y)

digits :: Parser String
digits = many1 digit

hashchar :: Parser Char
hashchar = char '#' >> oneOf "boxdtf\\"

-- Gets the value from a read function such as readHex
-- Since the parser handles parsing we know it will always
-- produce a value if the parser successes
getValue :: [(a, String)] -> a
getValue [(x,_)] = x
getValue _       = error "Should not happen"

hexToNum :: (Num a, Eq a) => String -> a
hexToNum = getValue . readHex

octToNum :: (Num a, Eq a) => String -> a
octToNum = getValue . readOct

binToNum :: (Num a, Eq a) => String -> a
binToNum = getValue . readBin

isBinChar :: Char -> Bool
isBinChar '0' = True
isBinChar '1' = True
isBinChar _   = False

binCharToInt :: Char -> Int
binCharToInt '0' = 0
binCharToInt '1' = 1
binCharToInt _   = error "Not a binary character"

readBin :: (Num a, Eq a) => ReadS a
readBin = readInt 2 isBinChar binCharToInt

binDigits :: Parser String
binDigits = many1 $ oneOf "01"

hexDigits :: Parser String
hexDigits = many1 hexDigit

octDigits :: Parser String
octDigits = many1 octDigit

parseNumberBin :: Parser LispVal
parseNumberBin = liftM (Integer . binToNum) binDigits

parseNumberHex :: Parser LispVal
parseNumberHex = liftM (Integer . hexToNum) hexDigits

parseNumberOct :: Parser LispVal
parseNumberOct = liftM (Integer . octToNum) octDigits

parseInteger :: Parser LispVal
parseInteger = liftM (Integer . read) digits

parseNumber :: Parser LispVal
parseNumber = try parseReal <|> try parseRational <|> try parseComplex <|> parseInteger

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseExpr :: Parser LispVal
parseExpr = parseHash
            <|> parseAtom
            <|> parseString
            <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val  -> "Found value: " ++ show val

main :: IO ()
main = getArgs >>= putStrLn . readExpr . head
