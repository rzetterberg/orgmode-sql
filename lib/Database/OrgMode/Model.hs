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

DateTime
    year Int
    month Int
    day Int
    hour Int
    minute Int
    deriving Show

Timestamp
    start DateTimeId
    active Bool
    end DateTimeId Maybe
    duration Int
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
    time TimestampId
    deriving Show

Clock
    heading HeadingId
    time TimestampId
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
