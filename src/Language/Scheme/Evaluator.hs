module Language.Scheme.Evaluator where

import Language.Scheme.Types
import Language.Scheme.Error.Types

import Control.Monad.Except (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("string?", typeTest "string?"),
              ("number?", typeTest "number?")]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params = mapM unpackNum params >>=
                         return . Number . Integer . foldl1 op

-- | type test primitive function
-- TODO: symbol? and others. Also make (string? "hi" 1 2 3) fail and not return false ...
typeTest :: String -> [LispVal] -> ThrowsError LispVal
typeTest "string?" [(String _)] = return $ Bool True
typeTest "number?" [(Number _)] = return $ Bool True
typeTest "symbol?" [(Atom _)]   = return $ Bool True

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number (Integer n)) = return n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
                       if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
