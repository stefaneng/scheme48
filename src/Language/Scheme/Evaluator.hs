module Language.Scheme.Evaluator where

import Language.Scheme.Types

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
