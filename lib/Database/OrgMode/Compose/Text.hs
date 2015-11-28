{-|
Contains 'Buildable' instances for all orgmode-parse types along with helper
functionality. When building a whole orgmode-parse 'Document' it will produce
the <http://orgmode.org/worg/dev/org-syntax.html org-mode plaintext format>.

To produce a strict 'Text' from a 'Document' just use the 'export' function
from this module.

In case you want to procude a lazy 'Text' you can use:

> import Data.Text.Buildable
> import Data.Text.Lazy.Builder
> toLazyText (build myDocument)

All 'export' does is to wrap that with strict conversion:

> export :: Document -> Text
> export = TL.toStrict . toLazyText . build
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Database.OrgMode.Compose.Text where

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
-- * Helpers

{-|
Exports the given document as a strict 'Text'.
-}
export :: Document -> Text
export = TL.toStrict . toLazyText . build

{-|
Builds a pair of digits ([0-9]{2}) and adds a 0 infront of the number if
it's under 10.

Used for printing out pairs in clocks 01:00 or dates 2015-01-05.

>>> toLazyText $ build (buildIntPair 4)
"04"
-}
buildIntPair :: (Integral a, Buildable a) => a -> Builder
buildIntPair v
    | v > 9     = build v
    | otherwise = build '0' `mappend` build v

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

>>> toLazyText $ build (buildSpaceM Nothing)
""
>>> toLazyText $ build (buildSpaceM (Just "hello"))
" hello"
-}
buildSpaceM :: (Buildable a) => Maybe a -> Builder
buildSpaceM Nothing  = mempty
buildSpaceM (Just a) = build a `mappend` singleton ' '

-------------------------------------------------------------------------------
-- * Instances

instance Buildable Document where
    build Document{..}
        = let text = if documentText == ""
                       then mempty
                       else fromText documentText `mappend` singleton '\n'
          in text `mappend` build documentHeadings

instance Buildable [Heading] where
    build []     = mempty
    build (h:[]) = build h
    build (h:hs)
        | null (subHeadings h) = build h `mappend` singleton '\n' `mappend` build hs
        | otherwise            = build h `mappend` build hs

instance Buildable Heading where
    build Heading{..}
        = mconcat [ build level
                  , singleton ' '
                  , buildSpaceM keyword
                  , buildSpaceM priority
                  , build title
                  , buildTags tags
                  , buildSection section
                  , buildSubs subHeadings
                  ]
      where
        buildTags [] = mempty
        buildTags ts = F.build " :{}:" $ Only (build ts)
        buildSection sec
            | sectionEmpty sec = mempty
            | otherwise        = singleton '\n' `mappend` build section
        buildSubs []   = mempty
        buildSubs subs = singleton '\n' `mappend` build subs

instance Buildable Section where
    build Section{..}
        = mconcat [ build sectionPlannings
                  , build sectionClocks
                  , build sectionProperties
                  , buildParagraph (T.strip sectionParagraph)
                  ]
      where
        buildParagraph p
            | T.null p  = mempty
            | otherwise = pad `mappend` build p

instance Buildable Plannings where
    build (Plns hmap) = case (HM.toList hmap) of
        [] -> mempty
        ls -> pad `mappend` build ls `mappend` singleton '\n'

instance Buildable Properties where
    build _ = mempty

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
        = F.build "{}CLOCK: {}{}{}"
          ( pad
          , buildClockTime tsTime
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
          ( buildIntPair year
          , buildIntPair month
          , buildIntPair day
          , buildSpaceM dayName
          , buildSpaceM hourMinute
          )
       where
         (YMD' (YearMonthDay year month day)) = yearMonthDay

instance Buildable (Int, Int) where
    build (h, m) = F.build "{}:{}" (buildIntPair h, buildIntPair m)

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
