{-|
Contains 'Buildable' instances for all orgmode-parse types along with helper
functionality. When building a whole orgmode-parse 'Document' it will produce
the <http://orgmode.org/worg/dev/org-syntax.html org-mode plaintext format>.

To produce a strict 'Text' from a 'Document' just use the 'export' function
from this module.

In case you want to procude a lazy 'Text' you can use:

> import Data.Text.Buildable
> import Data.Text.Lazy.Builder
> toLazyText (render myDocument)

All 'render' does is to wrap that with strict conversion:

> render :: Document -> Text
> render = TL.toStrict . toLazyText . mkDocument
-}

module Database.OrgMode.Render.OrgModeText where

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
render :: Document -> Text
render = TL.toStrict . toLazyText . mkDocument

-------------------------------------------------------------------------------
-- * Helpers

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
mkSpaceM :: Maybe a -> (a -> Builder) -> Builder
mkSpaceM Nothing  _ = mempty
mkSpaceM (Just a) f = f a `mappend` singleton ' '

mkWhen :: Bool -> Builder -> Builder
mkWhen True b  = b
mkWhen False _ = mempty

mkUnless :: Bool -> Builder -> Builder
mkUnless cond = mkWhen (not cond)

mkIsJust :: Maybe a -> (a -> Builder) -> Builder
mkIsJust Nothing _  = mempty
mkIsJust (Just a) f = f a

-------------------------------------------------------------------------------
-- * Instances

mkDocument :: Document -> Builder
mkDocument Document{..}
    = let text = mkUnless (documentText == "") $ fromText documentText `mappend` singleton '\n'
      in text `mappend` mkHeadings documentHeadings

mkHeadings :: [Heading] -> Builder
mkHeadings []     = mempty
mkHeadings (h:[]) = mkHeading h
mkHeadings (h:hs) = mkHeading h `mappend` divider `mappend` mkHeadings hs
  where
    divider = mkWhen (null (subHeadings h)) (singleton '\n')

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

mkPlannings :: Plannings -> Builder
mkPlannings (Plns hmap) = mkUnless (null ls) $
    pad `mappend` go ls `mappend` singleton '\n'
  where
    ls        = HM.toList hmap
    go []     = mempty
    go (p:[]) = mkPlanning p
    go (p:ps) = mkPlanning p `mappend` singleton ' ' `mappend` go ps

mkProperties :: Properties -> Builder
mkProperties _ = mempty

mkPlanning :: (PlanningKeyword, Timestamp) -> Builder
mkPlanning (kword, tstamp)
    = F.build "{}: <{}>"
      ( build (show kword)
      , mkDateTime tsTime
      )
  where
    Timestamp{..} = tstamp

mkClocks :: [(Maybe Timestamp, Maybe Duration)] -> Builder
mkClocks = mconcat . map (\c -> mkClock c `mappend` singleton '\n')

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
    mkEnd t = F.build "--{}" $ Only (mkClockTime t)
    mkDur d = F.build " =>  {}" $ Only (mkHourMinute d)

mkClockTime :: DateTime -> Builder
mkClockTime = F.build "[{}]" . Only . mkDateTime

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

mkHourMinute :: (Int, Int) -> Builder
mkHourMinute (h, m) = F.build "{}:{}" (mkIntPair h, mkIntPair m)

mkLevel :: Level -> Builder
mkLevel (Level len) = build $ TL.replicate (fromIntegral len) "*"

mkStateKeyword :: StateKeyword -> Builder
mkStateKeyword (StateKeyword val) = build val

mkPriority :: Priority -> Builder
mkPriority = F.build "[#{}]" . Only . build . show

mkTags :: [Tag] -> Builder
mkTags tags = mkUnless (null tags) $ F.build " :{}:" $ Only (go tags)
  where
    go []     = mempty
    go (t:[]) = build t
    go (t:ts) = build t `mappend` singleton ':' `mappend` go ts
