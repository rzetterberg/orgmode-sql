{-|
CRUD functionality for 'Property's.
-}
module Database.OrgMode.Query.Property where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key Heading
    -> Text
    -> Text
    -> ReaderT SqlBackend m (Key Property)
add owner key value = P.insert (Property owner key value)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrives all 'Property's by 'Heading' Id.

ASC sorted by clock start.
-}
getByHeading :: (MonadIO m)
             => Key Heading
             -> ReaderT SqlBackend m [Entity Property]
getByHeading hedId =
    select $
        from $ \(heading, property) -> do
            where_ (heading ^. HeadingId  ==. val hedId)
            where_ (property ^. PropertyHeading ==. heading ^. HeadingId)

            return property
