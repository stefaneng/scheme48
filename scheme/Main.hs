module Main where

import System.Environment (getArgs)
import Language.Scheme.Types
import Language.Scheme.Types.Instances
import Language.Scheme.Parser
import Language.Scheme.Evaluator
import Data.Complex (Complex((:+)))

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
