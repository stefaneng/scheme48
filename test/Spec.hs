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

evalString :: String -> ThrowsError LispVal
evalString strExpr = readExpr strExpr >>= eval

evalFull :: String -> String
evalFull = extractValue . trapError . liftM show . evalString
