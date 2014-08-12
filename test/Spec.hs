module Main where

import Test.Hspec

import Language.Scheme.Evaluator
import Language.Scheme.Parser
import Language.Scheme.Error
import Language.Scheme.Error.Types
import Language.Scheme.Types
import Control.Monad (liftM)

main :: IO ()
main = hspec $ do
  describe "Primitive Functions" $ do
    carSpec
    cdrSpec

-- | Spec for the `car` primitive
carSpec :: Spec
carSpec =
 describe "car" $ do
  it "should return the first element of an S-expr" $ do
    evalFull "(car '(a b c))" `shouldBe` "a"
  it "should return the first element of a dotted list" $ do
    evalFull "(car '(a b . c))" `shouldBe` "a"
 -- There really should be a better way to fail if it does not pattern match
  it "should report a type mismatch when argument is not a list" $ do
    case evalString "(car 'a)" of
      (Left (TypeMismatch _ _)) -> return ()
      x                         -> expectationFailure $
                               "Expected a type mismatch but got: " ++ show x
  it "should report a wrong number of arguments when number of args is not 1" $ do
    case evalString "(car 'a 'b)" of
      (Left (NumArgs 1 _)) -> return ()
      x                    -> expectationFailure $
                              "Expected a wrong number of args error but got: " ++ show x
  it "should fail when given an empty list" $ do
    case evalString "(car '())" of
      (Left (TypeMismatch _ _)) -> return ()
      x                         -> expectationFailure $
                                   "Expected to fail due to an empty list but got: " ++ show x

cdrSpec :: Spec
cdrSpec = do
  describe "cdr" $ do
   it "should return the tail of a pair" $ do
     evalFull "(cdr '(a b c))" `shouldBe` "(b c)"
     evalFull "(cdr '(a b))" `shouldBe` "(b)"
   it "should return an empty list (nil) when only one element" $ do
     evalFull "(cdr '(a))" `shouldBe` "()"
   it "should return the tail of a dotted pair" $ do
     evalFull "(cdr '(a . b))" `shouldBe` "b"
     evalFull "(cdr '(a b . c))" `shouldBe` "(b . c)"
   it "should fail when argument is not a pair" $ do
    case evalString "(cdr 'a)" of
      (Left (TypeMismatch _ _)) -> return ()
      x                         -> expectationFailure $
                                   "Expected a type mismatch but got: " ++ show x
   it "should fail when number of arguments is not 1" $ do
    case evalString "(cdr 'a 'b)" of
      (Left (NumArgs 1 _)) -> return ()
      x                    -> expectationFailure $
                              "Expected a wrong number of args error but got: " ++ show x

evalString :: String -> ThrowsError LispVal
evalString strExpr = readExpr strExpr >>= eval

evalFull :: String -> String
evalFull = extractValue . trapError . liftM show . evalString
