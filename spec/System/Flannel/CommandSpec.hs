module System.Flannel.CommandSpec
    ( spec
    ) where

import System.Flannel.Command
import qualified System.Flannel.Params as P
import Test.Hspec

spec :: Spec
spec = do
    describe "runCommand" $ do
        it "executes the command" $ do
            result <- runCommand P.defaultParams $ isSet "no set"
            result `shouldBe` False

    describe "isSet" $ do
        let params = P.setFlag "good" P.defaultParams

        context "when the flag is set" $ do
            it "returns True" $ do
                result <- runCommand params $ isSet "good"
                result `shouldBe` True

        context "when the flag is not set" $ do
            it "returns False" $ do
                result <- runCommand params $ isSet "bad"
                result `shouldBe` False

    describe "getOption" $ do
        let params = P.setOption "good" "alpha" P.defaultParams

        context "when the option is set" $ do
            it "returns the value" $ do
                result <- runCommand params $ getOption "good"
                result `shouldBe` Just "alpha"

        context "when the option is not set" $ do
            it "returns Nothing" $ do
                result <- runCommand params $ getOption "bad"
                result `shouldBe` Nothing

    describe "getArg" $ do
        let params = P.setArg "good" "1" P.defaultParams

        context "when the arg is set" $ do
            it "returns the value" $ do
                result <- runCommand params $ getArg "good"
                result `shouldBe` Just "1"

        context "when the arg is not set" $ do
            it "returns Nothing" $ do
                result <- runCommand params $ getArg "bad"
                result `shouldBe` Nothing

    describe "getRemaining" $ do
        let params = P.addRemaining ["1", "2"] P.defaultParams

        it "returns the remaining arguments" $ do
            result <- runCommand params getRemaining
            result `shouldBe` ["1", "2"]

    describe "run" $ do
        it "executes the IO action" $ do
            res <- runCommand P.defaultParams . run $ do
                fmap (head . lines) $ readFile "LICENSE"
            res `shouldBe` "The MIT License (MIT)"
