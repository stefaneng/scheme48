module Language.Scheme.Types where

import Data.Complex

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Bool Bool
             | Character Char
             | Number Number

data Number = Integer Integer
            | Real Double
            | Rational Rational
            | Complex (Complex Double)
