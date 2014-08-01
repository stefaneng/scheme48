module Main where

import Control.Monad (liftM)
import Numeric (readHex, readInt, readOct, readFloat)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Applicative ((<$>), (<*>))
import Data.Ratio
import Language.Scheme.Types
import Data.Complex (Complex((:+)))

instance Show Number where
    show = showNumber

instance Show LispVal where
    show = showVal

showNumber :: Number -> String
showNumber (Integer contents) = show contents
showNumber (Real contents) = show contents
showNumber (Rational contents) = show contents
showNumber (Complex contents) = show contents

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Character name) = [name]
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head' tail') = "(" ++ unwordsList head' ++ " . " ++ showVal tail' ++ ")"


unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

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
toDouble (Number (Real f))  = f
toDouble (Number (Integer n)) = fromIntegral n

parseReal :: Parser LispVal
parseReal = liftM (Number . Real . getDouble) float
    where float = (++) <$> digits <*> decimal
          decimal = (:) <$> char '.' <*> digits

parseRational :: Parser LispVal
parseRational = do
  numer <- digits
  char '/'
  denom <- digits
  return $ Number $ Rational ((read numer) % (read denom))

parseComplex :: Parser LispVal
parseComplex = do
  x <- (try parseReal <|> parseInteger)
  char '+'
  y <- (try parseReal <|> parseInteger)
  char 'i'
  return $ Number $ Complex (toDouble x :+ toDouble y)

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
parseNumberBin = liftM (Number . Integer . binToNum) binDigits

parseNumberHex :: Parser LispVal
parseNumberHex = liftM (Number . Integer . hexToNum) hexDigits

parseNumberOct :: Parser LispVal
parseNumberOct = liftM (Number . Integer . octToNum) octDigits

parseInteger :: Parser LispVal
parseInteger = liftM (Number . Integer . read) digits

parseNumber :: Parser LispVal
parseNumber = try parseReal <|> try parseRational <|> try parseComplex <|> parseInteger

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseHash
            <|> parseAtom
            <|> parseString
            <|> parseNumber
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> String $ "No match: " ++ show err
                   Right val  -> val

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _)   = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", typeTest "string?"),
              ("number?", typeTest "number?")]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number . Integer $ foldl1 op $ map unpackNum params

-- | type test primitive function
-- TODO: symbol? and others. Also make (string? "hi" 1 2 3) fail and not return false ...
typeTest :: String -> [LispVal] -> LispVal
typeTest "string?" [(String _)] = Bool True
typeTest "number?" [(Number _)] = Bool True
typeTest "symbol?" [(Atom _)]   = Bool True
typeTest _         _            = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number (Integer n)) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                       if null parsed
                          then 0
                          else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0


main :: IO ()
main = getArgs >>= print . eval . readExpr . head
