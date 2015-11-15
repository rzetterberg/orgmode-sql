{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.OrgMode.Marshall where

import           Prelude
import           Data.OrgMode.Parse.Types (Priority(..), PlanningKeyword(..))
import           Database.Persist.TH

--------------------------------------------------------------------------------

deriving instance Read PlanningKeyword

derivePersistField "Priority"
derivePersistField "PlanningKeyword"
