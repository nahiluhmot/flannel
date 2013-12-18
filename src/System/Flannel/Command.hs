-- | This module defines the command DSL.
module System.Flannel.Command
    ( CommandM
    , Command
    , runCommand
    , isSet
    , getOption
    , getArg
    , getRemaining
    , run
    ) where

import Control.Monad.Reader
import qualified System.Flannel.Params as P

-- | To build a Command, a 'ReaderT' is used. The type that may be read is
-- 'P.Params', and it wraps 'IO' so that actions may be performed.
type CommandM a = ReaderT P.Params IO a

-- | Alias for readability.
type Command = CommandM ()

-- | Run the 'Command' with the given 'P.Params'
runCommand :: P.Params -> CommandM a -> IO a
runCommand = flip runReaderT

-- | Test if a flag is set.
isSet :: String -> CommandM Bool
isSet = asks . P.isSet

-- | Retrieve an option.
getOption :: String -> CommandM (Maybe String)
getOption = asks . P.getOption

-- | Retrieve an argument by name.
getArg :: String -> CommandM (Maybe String)
getArg = asks . P.getArg

-- | Get the remaining arguments.
getRemaining :: CommandM [String]
getRemaining = asks P.getRemaining

-- | Run an IO action within a 'CommandM'.
run :: IO a -> CommandM a
run = liftIO
