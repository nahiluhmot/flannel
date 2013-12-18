module System.Flannel.CommandBuilderSpec
    ( spec
    ) where

import qualified System.Flannel.Argument as A
import System.Flannel.Command
import System.Flannel.CommandBuilder
import qualified System.Flannel.Params as P
import Test.Hspec

spec :: Spec
spec = do
    describe "desc" $ do
        it "sets the description" $ do
            let (BuilderState d _ _) = runCommandBuilder $ desc "good desc"
            d `shouldBe` "good desc"

    describe "arg" $ do
        it "adds a new argument" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    arg "file" "The file to read"
            args `shouldBe` [(A.Optional, A.Argument "file" "The file to read")]

    describe "requiredArg" $ do
        it "adds a new argument" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    requiredArg "f" "The file"
            args `shouldBe` [(A.Required, A.Argument "f" "The file")]

    describe "flag" $ do
        it "adds a new flag" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    flag "file" ["-f", "--file"] "The File"
                f = A.Flag "file" ["-f", "--file"] "The File"
            args `shouldBe` [(A.Optional, f)]

    describe "requiredFlag" $ do
        it "adds a new flag" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    requiredFlag "output" ["-o", "--output"] "The output"
                f = A.Flag "output" ["-o", "--output"] "The output"
            args `shouldBe` [(A.Required, f)]

    describe "option" $ do
        it "adds a new option" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    option "file" ["-f", "--file"] "The File"
                o = A.Option "file" ["-f", "--file"] "The File"
            args `shouldBe` [(A.Optional, o)]

    describe "requiredOption" $ do
        it "adds a new option" $ do
            let (BuilderState _ args _) = runCommandBuilder $ do
                    requiredOption "output" ["-o", "--output"] "The output"
                o = A.Option "output" ["-o", "--output"] "The output"
            args `shouldBe` [(A.Required, o)]

    describe "command" $ do
        it "adds the command" $ do
            let (BuilderState _ _ cmd) = runCommandBuilder $ do
                    command $ return ()
            result <- runCommand P.defaultParams cmd
            result `shouldBe` ()
