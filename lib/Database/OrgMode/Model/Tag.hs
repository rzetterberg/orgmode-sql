{-|
CRUD functionality for 'Tag's.
-}
module Database.OrgMode.Model.Tag where

import           Database.Persist

import           Database.OrgMode.Import
import           Database.OrgMode.Model

-------------------------------------------------------------------------------
-- * Creation

{-|
Adds the given tag name into the database and assigns the 'Heading' with
the given ID the owner of the tag.

Note that tag names are unique in the database so if the tag name already
exists that tag name will be used when creating the relationship between
the 'Heading' and the 'Tag'.
-}
add :: (MonadIO m) => Key Heading -> Text -> ReaderT SqlBackend m (Key Tag)
add headingId tagName = do
    currId <- getBy (UniqueTag tagName) >>= getCurrId

    void $ insert (TagRel headingId currId)

    return currId
  where
    getCurrId (Just (Entity currId _ )) = return currId
    getCurrId Nothing                   = insert (Tag tagName)

-------------------------------------------------------------------------------
-- * Retrieval

{-|
Retrieves all 'Tag's found in the database.

ASC sorted by name.
-}
getAll :: (MonadIO m) => ReaderT SqlBackend m [Entity Tag]
getAll = selectList [] [Asc TagName]
