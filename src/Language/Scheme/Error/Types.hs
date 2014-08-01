module Language.Scheme.Error.Types where

import Language.Scheme.Types
import Text.ParserCombinators.Parsec.Error (ParseError)

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
