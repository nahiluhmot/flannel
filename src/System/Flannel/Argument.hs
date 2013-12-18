module System.Flannel.Argument
    ( Argument(..)
    , Worth(..)
    ) where

-- | 'Argument' represents a specific type of command line argument.
data Argument
    -- | A simple argument that isn't preceeded by anything.
    = Argument String String
    -- | A flag that is low unless its set.
    | Flag String [String] String
    -- | An option that is qualified by a 'String'.
    | Option String [String] String
    deriving (Eq, Show)

-- | An 'Argument' is assigned a 'Worth', meaning it can either be
-- 'Required' or 'Optional'.
data Worth
    -- | Ensure that the 'Argument' is supplied.
    = Required
    -- | Do not ensure that the 'Argument' is supplied.
    | Optional
    deriving (Eq, Ord, Enum, Show)
