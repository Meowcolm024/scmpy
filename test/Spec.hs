import Test.Hspec 

import Parser

main :: IO ()
main = hspec spec

-- scheme -> python
transpile :: String -> String
transpile = undefined

spec = do
  describe "convertBase" $ do
    it "should work on simple examples" $ do
        parseLisp "(+ 1 2)" `shouldBe` (Right $ List [Atom "+", Number 1, Number 2])
