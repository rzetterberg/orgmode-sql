{-|
CRUD functionality for 'Timestamp's.
-}
module Database.OrgMode.Query.Timestamp where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key DateTime
    -> Bool
    -> Maybe (Key DateTime)
    -> Int
    -> ReaderT SqlBackend m (Key Timestamp)
add start active endM dur = P.insert $
    Timestamp start active endM dur
