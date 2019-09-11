import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import RecFunc
import Prelude hiding (and, concat, replicate, (!!), elem)

main :: IO ()
main = hspec $ do
    describe "#and" $ do
        it "when all elements are true" $ do 
            and [True, True] `shouldBe` True
        it "when the list is empty" $ do 
            and [] `shouldBe` True
        it "when the list has False" $ do 
            and [True, False, True] `shouldBe` False
    
    describe "#concat" $ do
        it "when list is not empty" $ do 
            concat [[1, 2], [3, 4]] `shouldBe` [1, 2, 3, 4]
        it "when list is empty" $ do
            (concat [] :: String) `shouldBe` ([] :: String)
        it "when different list sizes" $ do
            concat [[1, 2], [3]] `shouldBe` [1, 2, 3]

