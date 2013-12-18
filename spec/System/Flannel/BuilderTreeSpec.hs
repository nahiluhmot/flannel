module System.Flannel.BuilderTreeSpec
    ( spec
    ) where

import qualified System.Flannel.Argument as A
import System.Flannel.Command
import System.Flannel.CommandBuilder
import System.Flannel.BuilderTree
import Test.Hspec

spec :: Spec
spec = do
    describe "define" $ do
        context "when there is no previous definition" $ do
            let expected =
                    [ (A.Optional, A.Flag "beta" ["-b", "--beta"] "beta flag")
                    , (A.Optional, A.Argument "alpha" "the first argument")
                    ]

            it "defines the CommandBuilder" $ do
                let definition = runDsl $ do
                        define $ do
                            desc "description"
                            flag "beta" ["-b", "--beta"] "beta flag"
                            arg "alpha" "the first argument"
                            command $ do
                                run $ print "yay" 

                length definition `shouldBe` 1

                case head definition of
                    Namespace _ _ -> False `shouldBe` True
                    Builder b -> do
                        let BuilderState d a _ = runCommandBuilder b

                        d `shouldBe` "description"
                        a `shouldBe` expected

        context "when there is a previous definition" $ do
            it "does not overide it" $ do
                let definition = runDsl $ do
                        define $ do
                            desc "d1"

                        define $ do
                            desc "d2"

                length definition `shouldBe` 1

                case head definition of
                    Namespace _ _ -> False `shouldBe` True
                    Builder b -> do
                        let BuilderState d _ _ = runCommandBuilder b
                        
                        d `shouldBe` "d1"

    describe "namespace" $ do
        it "wraps a new Dsl" $ do
            let definition = runDsl $ do
                    namespace "new namespace" $ return ()

            length definition `shouldBe` 1

            case head definition of
                Builder _ -> False `shouldBe` True
                Namespace d _ ->
                    d `shouldBe` "new namespace"
