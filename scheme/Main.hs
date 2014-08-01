module Main where

import Language.Scheme.Types()
import Language.Scheme.Types.Instances()
import Language.Scheme.Parser
import Language.Scheme.Evaluator

import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
