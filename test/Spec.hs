import Test.Hspec 

import Parser

main :: IO ()
main = hspec spec

parseLisp = regularParse parseExpr

spec = do
  describe "convertBase" $ do
    it "should work on simple examples" $ do
        parseLisp "(+ 1 2)" `shouldBe` (Right $ List [Atom "+", Number 1, Number 2])
