module System.Flannel.ParserSpec
    ( spec
    ) where

import System.Flannel.Argument
import System.Flannel.CommandBuilder
import System.Flannel.BuilderTree
import System.Flannel.Params as P
import System.Flannel.Parser
import Test.Hspec
import qualified Text.Parsec as Parsec

spec :: Spec
spec = do
    describe "findRem" $ do
        context "when the predicate is satisfied" $ do
            it "returns the element with the rest of the list" $ do
                result <- findRem (return . odd) ([2,4,6,7,8,9] :: [Int])
                result `shouldBe` Just (7, [2,4,6,8,9])

        context "when the predicate is not satisfied" $ do
            it "returns Nothing" $ do
                result <- findRem (return . even) ([1,3,5,7,9] :: [Int])
                result `shouldBe` Nothing

    describe "wouldWork" $ do
        let parser = wouldWork $ string "str1"

        context "when the parser would work" $ do
            it "returns True" $ do
                runParser parser ["str1", "str2"] `shouldBe` Right True

            it "does not advance the parse" $ do
                let res = runParser (parser >> Parsec.getInput) ["str1", "str2"]
                res `shouldBe` Right ["str1", "str2"]

        context "when the parser would not work" $ do
            it "returns False" $ do
                runParser parser ["str3", "str4"] `shouldBe` Right False

            it "does not advance the parse" $ do
                let res = runParser (parser >> Parsec.getInput) ["str3", "str4"]
                res `shouldBe` Right ["str3", "str4"]

    describe "satisfy" $ do
        context "when the parse succeeds" $ do
            it "returns that argument" $ do
                case runParser (satisfy (`elem` ["a", "b"])) ["a"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right str -> str `shouldBe` "a"

        context "when the parse fails" $ do
            it "raises an error" $ do
                case runParser (satisfy (== "goat")) ["cow"] of
                    Left _ -> return () :: IO ()
                    Right _ -> fail "Expected the parse to fail"

    describe "anyString" $ do
        let parser = do
                str <- anyString
                str' <- anyString
                return (str, str')

        context "when there is input remaining" $ do
            it "returns the next String" $ do
                case runParser parser ["moo", "mee", "maa"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right (str, str') -> do
                        str `shouldBe` "moo"
                        str' `shouldBe` "mee"

        context "when the input has been used" $ do
            it "raises an error" $ do
                case runParser parser ["moo"] of
                    Left _ ->  return () :: IO ()
                    Right _ -> fail "Expected the parse to fail"

    describe "string" $ do
        context "when the input matches the specified String" $ do
            it "returns the next String" $ do
                case runParser (string "lol") ["lol", "lel"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right str -> str `shouldBe` "lol"

        context "when the input does not match the String" $ do
            it "raises an error" $ do
                case runParser (string "hello") ["world"] of
                    Left _ ->  return () :: IO ()
                    Right _ -> fail "Expected the parse to fail"

    describe "oneOf" $ do
        context "when one of the arguments matches" $ do
            it "returns the matching argument" $ do
                case runParser (oneOf ["alpha", "beta"]) ["beta"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right str -> str `shouldBe` "beta"

        context "when none of the arguments match" $ do
            it "raises an error" $ do
                case runParser (oneOf ["alpha", "beta"]) ["gamma"] of
                    Left _ -> return () :: IO ()
                    Right _ -> fail "Expected the parse to fail"

    describe "parseFlag" $ do
        context "when the input matches one of the flags" $ do
            it "sets the correct flag" $ do
                case runParser (parseFlag "file" ["-f", "--file"]) ["-f"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right val -> P.isSet "file" val `shouldBe` True

        context "when the input matches none of the flags" $ do
            it "raises an error" $ do
                case runParser (parseFlag "input" ["-i", "--in"]) ["-q"] of
                    Left _ -> return () :: IO ()
                    Right _ -> fail "Expected the parse to fail"

    describe "parseOption" $ do
        let parser = parseOption "out" ["-o", "--out"]

        context "when the prefix matches" $ do
            context "and there is an additional argument" $ do
                it "sets that argument" $ do
                    case runParser parser ["-o", "out.txt"] of
                        Left _ -> fail "Expected the parse to pass"
                        Right val ->
                            P.getOption "out" val `shouldBe` Just "out.txt"

            context "and there is not an additional argument" $ do
                it "raises an error" $ do
                    case runParser parser ["--out"] of
                        Left _ -> return () :: IO ()
                        Right _ -> fail "Expected the parse to pass"

        context "when the prefix does not match" $ do
            it "raises an error" $ do
                case runParser parser ["-a", "out.txt"] of
                    Left _ -> return () :: IO ()
                    Right _ -> fail "Expected the parse to pass"

    describe "parseArg" $ do
        let parser = parseArg "stream"
        context "when there is input remaining" $ do
            it "parses the next piece of input" $ do
                case runParser parser ["stdin"] of
                    Left _ -> fail "Expeceted the parse to pass"
                    Right val -> P.getArg "stream" val `shouldBe` Just "stdin"

        context "when there is no input remaining" $ do
            it "raises an error" $ do
                case runParser parser [] of
                    Left _ -> return () :: IO ()
                    Right _ -> fail "Expeceted the parse to pass"

    describe "parseArgument" $ do
        context "when the Argument is a argument" $ do
            let argument = Argument "in" ""

            it "attempts to parse an Argument" $ do
                case runParser (parseArgument argument) ["abc"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right val -> P.getArg "in" val `shouldBe` Just "abc"

        context "when the Argument is a flag" $ do
            let argument = Flag "input" ["-i", "--input"] ""

            it "attempts to parse a flag" $ do
                case runParser (parseArgument argument) ["-i"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right val -> P.isSet "input" val `shouldBe` True

        context "when the Argument is a option" $ do
            let argument = Option "output" ["-o", "--output"] ""

            it "attempts to parse an Option" $ do
                case runParser (parseArgument argument) ["-o", "alpha"] of
                    Left _ -> fail "Expected the parse to pass"
                    Right val -> P.getOption "output" val `shouldBe` Just "alpha"

    describe "parseWith" $ do
        context "when there are no namespaces" $ do
            context "when there are only arguments" $ do
                let forest = runDsl $ do
                        define $ do
                            arg "swag" "The first argument"
                            arg "swerve" "The second argument"
                    parse = parseWith forest
                    expectedArgs =
                        [ (Optional, Argument "swag" "The first argument")
                        , (Optional, Argument "swerve" "The second argument")
                        ]

                context "when there are no arguments" $ do
                    it "returns no arguments" $ do
                        case parse [] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.getArg "swag" params `shouldBe` Nothing
                                P.getArg "swerve" params `shouldBe` Nothing
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when there are too few arguments" $ do
                    it "parses as many as it can" $ do
                        case parse ["alpha"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.getArg "swag" params `shouldBe` Just "alpha"
                                P.getArg "swerve" params `shouldBe` Nothing
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when all of the arguments are parsed" $ do
                    it "returns the arguments" $ do
                        case parse ["alpha", "beta"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.getArg "swag" params `shouldBe` Just "alpha"
                                P.getArg "swerve" params `shouldBe` Just "beta"
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when there are too many arguments" $ do
                    it "puts the remaining arguments into a list" $ do
                        case parse ["alpha", "beta", "gamma", "delta"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.getArg "swag" params `shouldBe` Just "alpha"
                                P.getArg "swerve" params `shouldBe` Just "beta"
                                P.getRemaining params `shouldBe` ["gamma", "delta"]
                                args `shouldBe` expectedArgs

            context "when there are only flags" $ do
                let forest = runDsl $ do
                        define $ do
                            flag "input" ["-i", "--input"] "In"
                            flag "output" ["-o", "--output"] "Out"
                    parse = parseWith forest
                    expectedArgs =
                        [ (Optional, Flag "input" ["-i", "--input"] "In")
                        , (Optional, Flag "output" ["-o", "--output"] "Out")
                        ]

                context "when there are no arguments" $ do
                    it "sets no flags" $ do
                        case parse [] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.isSet "input" params `shouldBe` False
                                P.isSet "output" params `shouldBe` False
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when there is one matching argument" $ do
                    it "sets that flag" $ do
                        case parse ["--input"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.isSet "input" params `shouldBe` True
                                P.isSet "output" params `shouldBe` False
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when all of the flags are matched" $ do
                    it "sets the flags" $ do
                        case parse ["--input", "--output"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.isSet "input" params `shouldBe` True
                                P.isSet "output" params `shouldBe` True
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when an unexpected flag occurs before an expected one" $ do
                    it "puts the remaining arguments into remainingArgs" $ do
                        case parse ["--input", "--swag", "--output"] of
                            Left _ -> fail "Expected the parse to pass"
                            Right (args, params, _) -> do
                                P.isSet "input" params `shouldBe` True
                                P.isSet "output" params `shouldBe` False
                                P.getRemaining params `shouldBe`
                                    ["--swag", "--output"]
                                args `shouldBe` expectedArgs

            context "when there are mixed argument types" $ do
                let forest = runDsl $ do
                        define $ do
                            flag "verbose" ["-v", "--verbose"] "V"
                            option "in" ["-i", "--in"] "In file"
                            option "out" ["-o", "--out"] "Out file"
                            arg "target" "Target"
                    parse = parseWith forest
                    expectedArgs =
                        [ (Optional, Flag "verbose" ["-v", "--verbose"] "V")
                        , (Optional, Option "in" ["-i", "--in"] "In file")
                        , (Optional, Option "out" ["-o", "--out"] "Out file")
                        , (Optional, Argument "target" "Target")
                        ]

                context "when no arguments are provided" $ do
                    it "sets no parameters" $ do
                        case parse [] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` False
                                P.getOption "in" params `shouldBe` Nothing
                                P.getOption "out" params `shouldBe` Nothing
                                P.getArg "target" params `shouldBe` Nothing
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when a flag and argument are provided" $ do
                    it "sets the flag and argument" $ do
                        case parse ["-v", "abc"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` True
                                P.getOption "in" params `shouldBe` Nothing
                                P.getOption "out" params `shouldBe` Nothing
                                P.getArg "target" params `shouldBe` Just "abc"
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when an option and argument are provided" $ do
                    it "sets the flag and argument" $ do
                        case parse ["-o", "o.txt", "abc"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` False
                                P.getOption "in" params `shouldBe` Nothing
                                P.getOption "out" params `shouldBe` Just "o.txt"
                                P.getArg "target" params `shouldBe` Just "abc"
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs
                
                context "when two options are provided" $ do
                    it "sets the options" $ do
                        case parse ["-o", "o.txt", "-i", "in.txt"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` False
                                P.getOption "in" params `shouldBe` Just "in.txt"
                                P.getOption "out" params `shouldBe` Just "o.txt"
                                P.getArg "target" params `shouldBe` Nothing
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when only the argument is provided" $ do
                    it "sets the argument" $ do
                        case parse ["tar"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` False
                                P.getOption "in" params `shouldBe` Nothing
                                P.getOption "out" params `shouldBe` Nothing
                                P.getArg "target" params `shouldBe` Just "tar"
                                P.getRemaining params `shouldBe` []
                                args `shouldBe` expectedArgs

                context "when only the arg and extra args are provided" $ do
                    it "sets the argument" $ do
                        case parse ["tab", "swag", "lol"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` False
                                P.getOption "in" params `shouldBe` Nothing
                                P.getOption "out" params `shouldBe` Nothing
                                P.getArg "target" params `shouldBe` Just "tab"
                                P.getRemaining params `shouldBe` ["swag", "lol"]
                                args `shouldBe` expectedArgs

                context "when all of the flags, options, and args are provided" $ do
                    it "sets them all" $ do
                        case parse ["-i", "y", "-v", "-o", "z", "a", "b"] of
                            Left _ -> fail "Expected the prase to pass"
                            Right (args, params, _) -> do
                                P.isSet "verbose" params `shouldBe` True
                                P.getOption "in" params `shouldBe` Just "y"
                                P.getOption "out" params `shouldBe` Just "z"
                                P.getArg "target" params `shouldBe` Just "a"
                                P.getRemaining params `shouldBe` ["b"]
                                args `shouldBe` expectedArgs
