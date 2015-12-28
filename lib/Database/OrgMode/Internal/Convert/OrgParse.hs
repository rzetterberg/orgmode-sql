{-|
To produce a strict 'Text' from a orgmode-parse 'Document' just use the 'toText' function
from this module.
-}

module Database.OrgMode.Internal.Convert.OrgParse where

import           Data.OrgMode.Parse.Types
import           Data.Text (Text)
import           Data.Text.Buildable
import           Data.Text.Format (Only(..))
import           Data.Text.Lazy.Builder
import           Prelude
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Format as F
import qualified Data.Text.Lazy as TL

-------------------------------------------------------------------------------

{-|
Renders the given document as a strict 'Text'.
-}
toText :: Document -> Text
toText = TL.toStrict . toLazyText . mkDocument

-------------------------------------------------------------------------------
-- * Builder makers

{-|
Produces a 'Builder' for the given 'Document'.
-}
mkDocument :: Document -> Builder
mkDocument Document{..}
    = let text = mkUnless (documentText == "") $ fromText documentText `mappend` singleton '\n'
      in text `mappend` mkHeadings documentHeadings

{-|
Produces a 'Builder' for the given 'Heading's.
-}
mkHeadings :: [Heading] -> Builder
mkHeadings []     = mempty
mkHeadings (h:[]) = mkHeading h
mkHeadings (h:hs) = mkHeading h `mappend` divider `mappend` mkHeadings hs
  where
    divider = mkWhen (null (subHeadings h)) (singleton '\n')

{-|
Produces a 'Builder' for the given 'Heading'.
-}
mkHeading :: Heading -> Builder
mkHeading Heading{..}
    = mconcat [ mkLevel level
              , singleton ' '
              , mkSpaceM keyword mkStateKeyword
              , mkSpaceM priority mkPriority
              , build title
              , mkTags tags
              , mkSection section
              , mkUnless (null subHeadings)
                         (singleton '\n' `mappend` mkHeadings subHeadings)
              ]

{-|
Produces a 'Builder' for the given 'Section'.
-}
mkSection :: Section -> Builder
mkSection sec = mkUnless (sectionEmpty sec) $ singleton '\n' `mappend` go sec
  where
    go Section{..}
        = mconcat [ mkPlannings sectionPlannings
                  , mkClocks sectionClocks
                  , mkProperties sectionProperties
                  , mkParagraph (T.strip sectionParagraph)
                  ]
    mkParagraph p = mkUnless (T.null p) $ pad `mappend` build p

{-|
Produces a 'Builder' for the given 'Plannings'.
-}
mkPlannings :: Plannings -> Builder
mkPlannings (Plns hmap) = mkUnless (null ls) $
    pad `mappend` go ls `mappend` singleton '\n'
  where
    ls        = HM.toList hmap
    go []     = mempty
    go (p:[]) = mkPlanning p
    go (p:ps) = mkPlanning p `mappend` singleton ' ' `mappend` go ps

{-|
Produces a 'Builder' for the given 'Properties'.

NB: This function does nothing at the moment by only returning 'mempty'.
-}
mkProperties :: Properties -> Builder
mkProperties _ = mempty

{-|
Produces a 'Builder' for the given 'Plannings' tuple.
-}
mkPlanning :: (PlanningKeyword, Timestamp) -> Builder
mkPlanning (kword, tstamp)
    = F.build "{}: <{}>"
      ( build (show kword)
      , mkDateTime tsTime
      )
  where
    Timestamp{..} = tstamp

{-|
Produces a 'Builder' for the given Clocks.
-}
mkClocks :: [(Maybe Timestamp, Maybe Duration)] -> Builder
mkClocks = mconcat . map (\c -> mkClock c `mappend` singleton '\n')

{-|
Produces a 'Builder' for the given Clock tuple.
-}
mkClock :: (Maybe Timestamp, Maybe Duration) -> Builder
mkClock (Nothing, _) = mempty
mkClock (Just Timestamp{..}, durM)
    = F.build "{}CLOCK: {}{}{}"
      ( pad
      , mkClockTime tsTime
      , mkIsJust tsEndTime mkEnd
      , mkIsJust durM mkDur
      )
  where
    mkClockTime = F.build "[{}]" . Only . mkDateTime
    mkEnd t     = F.build "--{}" $ Only (mkClockTime t)
    mkDur d     = F.build " =>  {}" $ Only (mkHourMinute d)

{-|
Produces a 'Builder' for the given 'DateTime'.
-}
mkDateTime :: DateTime -> Builder
mkDateTime DateTime{..}
    = F.build "{}-{}-{}{}{}"
      ( mkIntPair year
      , mkIntPair month
      , mkIntPair day
      , mkSpaceM dayName build
      , mkSpaceM hourMinute mkHourMinute
      )
    where
      (YMD' (YearMonthDay year month day)) = yearMonthDay

{-|
Produces a 'Builder' for the given tuple of hour and minute as 'Int's.
-}
mkHourMinute :: (Int, Int) -> Builder
mkHourMinute (h, m) = F.build "{}:{}" (mkIntPair h, mkIntPair m)

{-|
Produces a 'Builder' for the given 'Level'.
-}
mkLevel :: Level -> Builder
mkLevel (Level len) = build $ TL.replicate (fromIntegral len) "*"

{-|
Produces a 'Builder' for the given 'StateKeyword'.
-}
mkStateKeyword :: StateKeyword -> Builder
mkStateKeyword (StateKeyword val) = build val

{-|
Produces a 'Builder' for the given 'Priority'.
-}
mkPriority :: Priority -> Builder
mkPriority = F.build "[#{}]" . Only . build . show

{-|
Produces a 'Builder' for the given 'Tag's.
-}
mkTags :: [Tag] -> Builder
mkTags tags = mkUnless (null tags) $ F.build " :{}:" $ Only (go tags)
  where
    go []     = mempty
    go (t:[]) = build t
    go (t:ts) = build t `mappend` singleton ':' `mappend` go ts

-------------------------------------------------------------------------------
-- * Helpers

{-|
Checks if the given orgmode-parse 'Section' is empty. Used for to determine
how linebreaks should be added for a 'Heading'.

>>> sectionEmpty $ Section (fromList []) [] (fromList []) ""
True
>>> sectionEmpty $ Section (fromList []) [] (fromList []) "Section paragraph"
False
-}
sectionEmpty :: Section -> Bool
sectionEmpty Section{..} =  HM.null plns
                         && HM.null sectionProperties
                         && null sectionClocks
                         && sectionParagraph == ""
  where
    (Plns plns) = sectionPlannings

{-|
Builds a pair of digits ([0-9]{2}) and adds a 0 infront of the number if
it's under 10.

Used for printing out pairs in clocks 01:00 or dates 2015-01-05.

>>> toLazyText $ build (mkIntPair 4)
"04"
-}
mkIntPair :: (Integral a, Buildable a) => a -> Builder
mkIntPair v
    | v > 9     = build v
    | otherwise = build '0' `mappend` build v

{-|
Helper for adding consistent padding to lines inside a 'Heading'. For example
adding padding infront of plannings:

> * This a task
>    DEADLINE: <2015-10-10 Sun>
-}
pad :: Builder
pad = fromText "   "

{-|
Helper for adding a space before a part that is 'Just'.

Used for when you have a line of multiple parts that needs space between
them but you want to avoid adding a spaces to parts that are 'Nothing'.
-}
mkSpaceM :: Maybe a -> (a -> Builder) -> Builder
mkSpaceM Nothing  _ = mempty
mkSpaceM (Just a) f = f a `mappend` singleton ' '

{-|
Helper for adding a builder when the given constraint is True.
Returns 'mempty' when False.
-}
mkWhen :: Bool -> Builder -> Builder
mkWhen True b  = b
mkWhen False _ = mempty

{-|
Helper for adding a builder when the given constraint is False.
Returns 'mempty' when True.
-}
mkUnless :: Bool -> Builder -> Builder
mkUnless cond = mkWhen (not cond)

{-|
Helper for running a function on the given value inside a 'Maybe' that
produces a 'Builder'.

Returns 'mempty' when given value is 'Nothing'.
-}
mkIsJust :: Maybe a -> (a -> Builder) -> Builder
mkIsJust Nothing _  = mempty
mkIsJust (Just a) f = f a
