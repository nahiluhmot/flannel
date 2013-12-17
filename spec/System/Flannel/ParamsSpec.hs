module System.Flannel.ParamsSpec
    ( spec
    ) where

import System.Flannel.Params
import Test.Hspec

spec :: Spec
spec = do
    describe "defaultParams" $ do
        it "sets every field as empty" $ do
            isSet "test" defaultParams `shouldBe` False
            getOption "test" defaultParams `shouldBe` Nothing
            getArg "test" defaultParams `shouldBe` Nothing
            getRemaining defaultParams `shouldBe` []
    
    describe "setFlag" $ do
        let params = setFlag "test" defaultParams

        it "sets the specified flag" $ do
            isSet "test" params `shouldBe` True

    describe "setOption" $ do
        let params = setOption "test" "alpha" defaultParams

        it "sets the specified option" $ do
            getOption "test" params `shouldBe` Just "alpha"

    describe "setArg" $ do
        let params = setArg "test" "beta" defaultParams

        it "sets the specified argument" $ do
            getArg "test" params `shouldBe` Just "beta"

    describe "addRemaining" $ do
        let params = addRemaining ["1", "2"] defaultParams

        it "adds the arguments" $ do
            getRemaining params `shouldBe` ["1", "2"]
