{-|
Contains functionality to convert data between different types such as
orgmode-parse data to internal data in this library, different types of
representation of time.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.OrgMode.Internal.PersistDerive where

import           Prelude
import           Data.OrgMode.Parse.Types (Priority(..), PlanningKeyword(..))
import           Database.Persist.TH

--------------------------------------------------------------------------------

deriving instance Read PlanningKeyword

derivePersistField "Priority"
derivePersistField "PlanningKeyword"
