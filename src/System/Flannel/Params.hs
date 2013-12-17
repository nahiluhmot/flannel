-- | This module defines the 'Params' data type. Its type constructor is not
-- exported in case I decide to change the implementation.
module System.Flannel.Params
    ( Params
    , defaultParams
    , setFlag
    , setOption
    , setArg
    , addRemaining
    , isSet
    , getOption
    , getArg
    , getRemaining
    ) where

import Control.Monad
import qualified Data.Map as M

-- | 'Params' is a mapping of the flags, options, arguments, and remaining args
-- passed in via the CLI.
data Params = Params { flags         :: M.Map String Bool
                     , options       :: M.Map String (Maybe String)
                     , arguments     :: M.Map String (Maybe String)
                     , remainingArgs :: [String]
                     } deriving (Eq, Show)

-- | By default, all 'flags', 'options', 'arguments', and 'remainingArgs' are
-- empty.
defaultParams :: Params
defaultParams = Params M.empty M.empty M.empty []

-- | Set a flag to 'True'.
setFlag :: String -> Params -> Params
setFlag str ps = ps { flags = M.insert str True (flags ps) }

-- | Set the specified option.
setOption :: String -> String -> Params -> Params
setOption k v ps = ps { options = M.insert k (Just v) (options ps) }

-- | Set the specified argument.
setArg :: String -> String -> Params -> Params
setArg k v ps = ps { arguments = M.insert k (Just v) (arguments ps) }

-- | Add the remaining arguments.
addRemaining :: [String] -> Params -> Params
addRemaining r ps = ps { remainingArgs = (remainingArgs ps) ++ r }

-- | Test if a flag is set.
isSet :: String -> Params -> Bool
isSet str params =
    case M.lookup str (flags params) of
        Nothing -> False
        Just v -> v

-- | Get the specified option.
getOption :: String -> Params -> Maybe String
getOption str params = join . M.lookup str . options $ params

-- | Get the specified argument.
getArg :: String -> Params -> Maybe String
getArg str params = join . M.lookup str . arguments $ params

-- | Get the remaining arguments.
getRemaining :: Params -> [String]
getRemaining = remainingArgs
