module Database.OrgMode.Model where

import           Data.OrgMode.Parse.Types (Priority(..), PlanningKeyword(..))

import           Database.OrgMode.Import
import           Database.OrgMode.Marshall()

--------------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Document
    name Text
    text Text
    UniqueDocument name
    deriving Show

Heading
    document DocumentId
    parent HeadingId Maybe
    level Int
    keyword Text Maybe
    priority Priority Maybe
    title Text
    paragraph Text
    deriving Show

Property
    heading HeadingId
    key Text
    value Text
    deriving Show

Planning
    heading HeadingId
    keyword PlanningKeyword
    time UTCTime
    deriving Show

Clock
    heading HeadingId
    active Bool
    start UTCTime
    end UTCTime Maybe
    duration Int
    deriving Show

Tag
    name Text
    UniqueTag name
    deriving Show

TagRel
    heading HeadingId
    item TagId
    deriving Show
|]
