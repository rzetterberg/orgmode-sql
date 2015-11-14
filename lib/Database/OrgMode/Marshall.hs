{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.OrgMode.Marshall where

import           Data.OrgMode.Parse.Types (Priority(..))
import           Database.Persist.TH

derivePersistField "Priority"
