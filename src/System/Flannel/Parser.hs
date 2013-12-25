-- | This module contains a generator for a 'BuilderForest'. Using 'parseWith'
-- will transform the 'BuilderForest' into a parser.
module System.Flannel.Parser
    ( Parser
    , findRem
    , wouldWork
    , runParser
    , satisfy
    , anyString
    , string
    , oneOf
    , parseFlag
    , parseOption
    , parseArg
    , parseArgument
    , parseWith
    ) where

import Control.Monad
import qualified Data.List as L
import Data.Monoid
import System.Flannel.Argument
import System.Flannel.BuilderTree
import System.Flannel.Command
import System.Flannel.CommandBuilder
import qualified System.Flannel.Params as PR
import qualified Text.Parsec as P
import qualified Text.Parsec.Pos as Pos

-- | Much like the 'P.Parser' type defined in Parsec, but accepts a list of
-- 'String's instead of a list af 'Char's.
type Parser a = P.Parsec [String] String a

-- | Given a monadic predicate, return the first satisfying element in the list
-- along with the rest of that list.
findRem :: Monad m => (a -> m Bool) -> [a] -> m (Maybe (a, [a]))
findRem f =
    let go _ [] = return Nothing
        go acc (x:xs) = do
            result <- f x
            if result then
                return $ Just (x, reverse acc ++ xs)
            else 
                go (x : acc) xs
    in  go []

-- | Without consuming any input, try if a 'Parser' would work.
wouldWork :: Parser a -> Parser Bool
wouldWork p = do
    state <- P.getInput
    res <- P.many p
    P.setInput state
    return . not $ null res

-- | Given a predicate, creates a 'Parser' for a satisfying 'String'.
satisfy :: (String -> Bool) -> Parser String
satisfy f =
    P.tokenPrim
        id 
        (\pos str _ -> Pos.updatePosString pos (' ' : str))
        (\str -> if f str then Just str else Nothing)

-- | Parse any 'String'.
anyString :: Parser String
anyString = satisfy $ const True

-- | Parse the exact given 'String.'
string :: String -> Parser String
string str = satisfy (== str)

-- | Parse one of the given 'String's.
oneOf :: [String] -> Parser String
oneOf = satisfy . flip elem

-- | Given a name and list of accepted flags, attempts to parse a flag.
parseFlag :: String -> [String] -> Parser PR.Params
parseFlag name flags = do
    _ <- oneOf flags
    return $ PR.setFlag name PR.defaultParams

-- | Given a name and list of accepted options, attempts to parse an option.
parseOption :: String -> [String] -> Parser PR.Params
parseOption name opts = do
    _ <- oneOf opts
    value <- anyString
    return $ PR.setOption name value PR.defaultParams

-- | Parse any 'String' with the given name.
parseArg :: String -> Parser PR.Params
parseArg name = do
    value <- anyString
    return $ PR.setArg name value PR.defaultParams

-- | Parse an argument.
parseArgument :: Argument -> Parser PR.Params
parseArgument (Argument name _) = parseArg name
parseArgument (Flag name fs _) = parseFlag name fs
parseArgument (Option name os _) = parseOption name os

-- | Given a list of flags and options, as well as a list of arguments, attempt
-- to parse the command line parameters.
parseArguments :: [Parser PR.Params] -> [Parser PR.Params] -> Parser PR.Params
parseArguments =
    let go params [] [] = do
            varArgs <- P.many anyString
            return (PR.addRemaining varArgs PR.defaultParams `mappend` params)
        go params [] args = do
            len <- fmap length P.getInput
            if len > length args then do
                ps <- fmap mconcat $ sequence args
                varArgs <- P.many anyString
                return $ PR.addRemaining varArgs ps `mappend` params
            else do
                ps <- fmap mconcat $ sequence (take len args)
                return $ ps `mappend` params
        go params flags args = do
            result <- findRem wouldWork flags
            case result of
                Nothing -> go params [] args
                Just (f, fs) -> do
                    p <- f
                    go (p `mappend` params) fs args
    in  go PR.defaultParams

-- | Given a 'BuilderState' creates a 'Parser'.
parseBuilderState ::
    BuilderState ->
    Parser ([(Worth, Argument)], PR.Params, Command)
parseBuilderState (BuilderState _ args cmd) = do
    let isArg (Argument _ _) = True
        isArg _ = False
        (as, fs) = L.partition isArg $ map snd args
    params <- parseArguments (map parseArgument fs) (map parseArgument as)
    return (args, params, cmd)

-- | Given a 'BuilderForest' creates a 'Parser'.
parseForest :: BuilderForest -> Parser ([(Worth, Argument)], PR.Params, Command)
parseForest ts = do
    let isBuilder (Builder _) = True
        isBuilder _ = False
        (bs, ns) = L.partition isBuilder ts
        (Builder b) = head bs
        parseNamespace (Namespace name f) = string name >> parseForest f
        parseNamespace _ = P.parserZero
        parseNs = foldr (P.<|>) P.parserZero $ map (P.try . parseNamespace) ns
        parseB | null bs = P.parserZero
               | otherwise = parseBuilderState b

    when (length bs < 1) $ do
        fail $ "Invalid number of definitions: " ++ show (length bs)

    parseNs P.<|> parseB

-- | Run the given parser.
runParser :: Parser a -> [String] -> Either String a
runParser p args =
    case P.runParser p "" "Arguments" args of
        Right v -> Right v
        Left err -> Left $ show err

-- | Given a 'BuilderForest' and list of arguments, will either parse it or
-- return an error.
parseWith ::
    BuilderForest ->
    [String] ->
    Either String ([(Worth, Argument)], PR.Params, Command)
parseWith = runParser . parseForest
