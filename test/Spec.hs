import Test.Hspec

main :: IO ()
main = hspec $ do
         describe "Example Test" $ do
                 it "returns the first element of a list" $ do
                                     head [23 ..] `shouldBe` (23 :: Int)
