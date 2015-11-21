{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.OrgMode.Export.Text where

import           Data.OrgMode.Parse.Types
import           Data.Text (Text)
import           Data.Text.Buildable
import           Data.Text.Lazy.Builder
import           Data.Text.Format (Only(..))
import qualified Data.Text.Format as F
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import           Prelude

-------------------------------------------------------------------------------

export :: Document -> Text
export = TL.toStrict . toLazyText . build

stripLinebreak :: Text -> Text
stripLinebreak a = maybe a id $
    T.stripSuffix "\n" a >>= T.stripPrefix "\n"

-------------------------------------------------------------------------------

instance Buildable Document where
    build Document{..}
        = let text = if documentText == ""
                       then mempty
                       else fromText documentText
          in text `mappend` build documentHeadings

instance Buildable [Heading] where
    build []     = mempty
    build (h:[]) = build h
    build (h:hs) = build h `mappend` singleton '\n' `mappend` build hs

instance Buildable Heading where
    build Heading{..}
        = F.build "{} {} {} {} :{}:\n{}\n{}"
          ( build level
          , build keyword
          , build priority
          , build title
          , build tags
          , build section
          , build subHeadings
          )

instance Buildable Section where
    build Section{..}
        = F.build "{}{}{}{}"
          ( build sectionPlannings
          , build sectionClocks
          , build sectionProperties
          , build (stripLinebreak sectionParagraph)
          )

instance Buildable Plannings where
    build (Plns hmap) = case (HM.toList hmap) of
        [] -> mempty
        ls -> fromText "  " `mappend` build ls `mappend` singleton '\n'

instance Buildable Properties where
    build _ = build ("" :: Text)

instance Buildable [(PlanningKeyword, Timestamp)] where
    build []     = mempty
    build (p:[]) = build p
    build (p:ps) = build p `mappend` singleton ' ' `mappend` build ps

instance Buildable (PlanningKeyword, Timestamp) where
    build (kword, tstamp)
        = F.build "{}: <{}>"
          ( build (show kword)
          , build tsTime
          )
      where
        Timestamp{..} = tstamp

instance Buildable [(Maybe Timestamp, Maybe Duration)] where
    build []     = mempty
    build (c:cs) = build c `mappend` singleton '\n' `mappend` build cs

instance Buildable (Maybe Timestamp, Maybe Duration) where
    build (Nothing, _) = mempty
    build (Just tstamp, durM)
        = F.build "  CLOCK: {}{}{}"
          ( buildClockTime tsTime
          , buildEnd tsEndTime
          , buildDur durM tsEndTime
          )
      where
        Timestamp{..}       = tstamp
        buildClockTime      = F.build "[{}]" . Only . build
        buildEnd Nothing    = mempty
        buildEnd (Just t)   = F.build "--{}" $ Only (buildClockTime t)
        buildDur Nothing _  = mempty
        buildDur _ Nothing  = mempty
        buildDur (Just d) _ = F.build " =>  {}" $ Only (build d)

instance Buildable DateTime where
    build DateTime{..}
        = F.build "{}-{}-{}{}{}"
          ( build year
          , build month
          , build day
          , buildDayname dayName
          , buildHourMinute hourMinute
          )
       where
         buildDayname Nothing      = mempty
         buildDayname (Just dn)    = singleton ' ' `mappend` build dn
         buildHourMinute Nothing   = mempty
         buildHourMinute (Just hm) = singleton ' ' `mappend` build hm
         (YMD' (YearMonthDay year month day)) = yearMonthDay

instance Buildable (Int, Int) where
    build (h, m) = F.build "{}:{}" (build h, build m)

instance Buildable Level where
    build (Level len) = build $ TL.replicate (fromIntegral len) "*"

instance Buildable StateKeyword where
    build (StateKeyword val) = build val

instance Buildable Priority where
    build = F.build "[#{}]" . Only . build . show

instance Buildable [Tag] where
    build []     = mempty
    build (t:[]) = build t
    build (t:ts) = build t `mappend` singleton ':' `mappend` build ts
