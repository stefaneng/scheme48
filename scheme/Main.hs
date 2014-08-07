module Main where

import Language.Scheme.Types()
import Language.Scheme.Parser
import Language.Scheme.Evaluator
import Language.Scheme.Error

import System.Environment (getArgs)
import Control.Monad (liftM)

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
