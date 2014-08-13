module Language.Scheme.Evaluator where

import Language.Scheme.Types
import Language.Scheme.Error.Types

import Control.Monad.Except (throwError)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _)   = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, conseq, alt]) = do
  result <- eval cond
  case result of
    Bool False -> eval alt
    otherwise  -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
 ("+", numericBinop (+)),
 ("-", numericBinop (-)),
 ("*", numericBinop (*)),
 ("/", numericBinop div),
 ("mod", numericBinop mod),
 ("quotient", numericBinop quot),
 ("remainder", numericBinop rem),
 -- Type test functions
 ("string?", typeTest "string?"),
 ("number?", typeTest "number?"),
 -- boolBinop functions
 ("=", numBoolBinop (==)),
 ("<", numBoolBinop (<)),
 (">", numBoolBinop (>)),
 ("/=", numBoolBinop (/=)),
 (">=", numBoolBinop (>=)),
 ("<=", numBoolBinop (<=)),
 ("&&", boolBoolBinop (&&)),
 ("||", boolBoolBinop (||)),
 ("string=?", strBoolBinop (==)),
 ("string<?", strBoolBinop (<)),
 ("string>?", strBoolBinop (>)),
 ("string<=?", strBoolBinop (<=)),
 ("string>=?", strBoolBinop (>=)),
 ("car", car),
 ("cdr", cdr),
 ("cons", cons)]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                               left <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ Bool $ left `op` right

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ []              = throwError $ NumArgs 2 []
numericBinop _ singleVal @[_]  = throwError $ NumArgs 2 singleVal
numericBinop op params         = mapM unpackNum params >>=
                                 return . Number . Integer . foldl1 op

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)]          = return x
car [DottedList (x : _) _]  = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)]         = return $ List xs
cdr [DottedList [_] x ]     = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs]  = return . List $ x : xs
cons [x, y]        = return $ DottedList [x] y
cons badArgList    = throwError $ NumArgs 2 badArgList

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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
