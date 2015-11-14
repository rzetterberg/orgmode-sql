{-|
CRUD functionality for 'DateTime's.
-}
module Database.OrgMode.Query.DateTime where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m) => DateTime -> ReaderT SqlBackend m (Key DateTime)
add = P.insert
