{-|
CRUD functionality for 'TagRel's.
-}
module Database.OrgMode.Query.TagRel where

import           Database.Esqueleto
import qualified Database.Persist as P

import           Database.OrgMode.Internal.Import
import           Database.OrgMode.Types

-------------------------------------------------------------------------------
-- * Creation

add :: (MonadIO m)
    => Key Heading
    -> Key Tag
    -> ReaderT SqlBackend m (Key TagRel)
add owner tag = P.insert (TagRel owner tag)
