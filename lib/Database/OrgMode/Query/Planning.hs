{-|
CRUD functionality for 'Planning's.
-}
module Database.OrgMode.Query.Planning where

import           Data.OrgMode.Parse.Types (PlanningKeyword(..))
import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key Heading
    -> PlanningKeyword
    -> Key Timestamp
    -> ReaderT SqlBackend m (Key Planning)
add owner keyword time = P.insert (Planning owner keyword time)
