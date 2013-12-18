-- | This module defines the top-level DSL.
module System.Flannel.BuilderTree
    ( BuilderTree(..)
    , BuilderForest
    , DslM
    , Dsl
    , define
    , namespace
    , runDsl
    ) where

import Control.Monad
import Control.Monad.State
import System.Flannel.CommandBuilder

-- | Each namespace has its own builder that will be executed when it is
-- matched.
data BuilderTree = Builder CommandBuilder 
                 | Namespace String BuilderForest

-- | A list of 'BuilderTree's.
type BuilderForest = [BuilderTree]

-- | Allows the building of custom command line interfaces.
type DslM a = State BuilderForest a

-- | Synonym for convenience.
type Dsl = DslM ()

-- | Define what happens when ARGV matches this namespace.
define :: CommandBuilder -> Dsl
define b = do
    forest <- get
    let f (Builder _) = True
        f _ = False
    when (null $ filter f forest) . put $ Builder b : forest

-- | Define a new namespace.
namespace :: String -> Dsl -> Dsl
namespace str dsl = modify (Namespace str (runDsl dsl) :)

-- | Run the 'Dsl'.
runDsl :: Dsl -> BuilderForest
runDsl = flip execState []
