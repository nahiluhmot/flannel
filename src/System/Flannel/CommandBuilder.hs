-- | This module holds the DSL that allows the specification of arguments,
-- flags, and options for a command.
module System.Flannel.CommandBuilder
    ( BuilderState(..)
    , CommandBuilderM
    , CommandBuilder
    , desc
    , arg
    , requiredArg
    , flag
    , requiredFlag
    , option
    , requiredOption
    , command
    , runCommandBuilder
    ) where

import Control.Monad.Writer
import qualified System.Flannel.Argument as A
import qualified System.Flannel.Command as C

-- | This Monoid holds the description, arguments, and command written by the
-- DSL.
data BuilderState = BuilderState String [(A.Worth, A.Argument)] C.Command

-- | This MonadWriter is used to build a 'BuilderState'.
type CommandBuilderM a = Writer BuilderState a

-- | Alias for readability.
type CommandBuilder = CommandBuilderM ()

instance Monoid BuilderState where
    mempty = BuilderState "" [] (return ())
    (BuilderState d1 a1 c1) `mappend` (BuilderState d2 a2 c2) =
        BuilderState (d1 ++ d2) (a1 ++ a2) (c1 >> c2)

-- | Set the description.
desc :: String -> CommandBuilder
desc str = tell $ BuilderState str [] (return ())

-- | Declare an optional argument.
arg :: String -> String -> CommandBuilder
arg = arg' A.Optional

-- | Declare a required argument.
requiredArg :: String -> String -> CommandBuilder
requiredArg = arg' A.Required

-- | Declare an optional flag.
flag :: String -> [String] -> String -> CommandBuilder
flag = flag' A.Optional

-- | Declare a required flag.
requiredFlag :: String -> [String] -> String -> CommandBuilder
requiredFlag = flag' A.Required

-- | Declare an optional option.
option :: String -> [String] -> String -> CommandBuilder
option = option' A.Optional

-- | Declare a required option.
requiredOption :: String -> [String] -> String -> CommandBuilder
requiredOption = option' A.Required

-- | Declare the 'C.Command' to be run.
command :: C.Command -> CommandBuilder
command = tell . BuilderState "" []

-- | Execute the 'CommmandBuilder'.
runCommandBuilder :: CommandBuilder -> BuilderState
runCommandBuilder = snd . runWriter

-- The following functions are not exported, but used as a DRY convenience.

arg' :: A.Worth -> String -> String -> CommandBuilder
arg' w name d =
    tell $ BuilderState "" [(w, A.Argument name d)] (return ())

flag' :: A.Worth -> String -> [String] -> String -> CommandBuilder
flag' w name flags d =
    tell $ BuilderState "" [(w, A.Flag name flags d)] (return ())

option' :: A.Worth -> String -> [String] -> String -> CommandBuilder
option' w name opts d =
    tell $ BuilderState "" [(w, A.Option name opts d)] (return ())
