
module Doc.SignatoryIdentification
  ( SignatoryIdentifierMap
  , SignatoryIdentifier(..)
  , signatoryIdentifierMap
  , signatoryIdentifier
  ) where

import Data.Function (on)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

import Doc.DocStateData
  ( Document, SignatoryLink, documentsignatorylinks, signatoryisauthor
  , signatorylinkid
  )
import Doc.SignatoryLinkID (SignatoryLinkID)
import Util.HasSomeUserInfo (getFullName)
import Util.SignatoryLinkUtils (isApprover, isSignatory)

type SignatoryIdentifierMap = Map SignatoryLinkID SignatoryIdentifier

data SignatoryIdentifier = SignatoryIdentifier
  { siLink     :: Maybe SignatoryLink
  , siFullName :: Text
  , siInitials :: Text
  }
  deriving Show

-- | Return the full name plus unique initials for a signatory
signatoryIdentifier :: SignatoryIdentifierMap -> SignatoryLinkID -> Text -> Maybe Text
signatoryIdentifier sim slid emptyNamePlaceholder = do
  si <- Map.lookup slid sim
  return
    $  (if T.null (siFullName si) then emptyNamePlaceholder else siFullName si)
    <> " ("
    <> siInitials si
    <> ")"

-- | Create a map with unique identifiers (name, initials) over a
-- sequence of documents, stemming from restarted signing processes.
-- When more than one signatory has the same initials, append a unique
-- number.  IDs for all signing parties that will be looked up must be
-- provided to guarantee unique identifiers for signatories that may
-- be missing due to deleted documents.
--
-- The author is always included even if not signing (but only the
-- author link associated with the last document in the list).  This
-- is to support the "Initiator" box on the verification page.
signatoryIdentifierMap
  :: Bool -> [Document] -> Set SignatoryLinkID -> SignatoryIdentifierMap
signatoryIdentifierMap includenonsigning docs slids = Map.fromList $ zipWith3
  (\esl name is ->
    let sl = esignatorylinkid esl
    in  (sl, SignatoryIdentifier (Map.lookup sl slmap) name is)
  )
  slsAndMissing
  names
  initials
  where
    esignatorylinkid = either signatorylinkid identity

    slmap =
      Map.fromList [ (signatorylinkid s, s) | d <- docs, s <- documentsignatorylinks d ]
    names = map (either getFullName (const "")) slsAndMissing
    -- Get initial of each name words, e.g. "John Smith" to "JS"
    initials =
      uniqueStrings $ enumerateEmpty $ map (T.concat . map (T.take 1) . T.words) names
    sls =
      filter
          (\s -> includenonsigning || isSignatory s || isApprover s || signatoryisauthor s
          )
        $ concatMap documentsignatorylinks docs
    slsAndMissing =
      sortBy (compare `on` esignatorylinkid) $ (map Left sls) ++ (map Right missing)
    missing = Set.toList $ slids Set.\\ Set.fromList (map signatorylinkid sls)

-- | Replace all empty strings in a list with "1", "2", ...
enumerateEmpty :: [Text] -> [Text]
enumerateEmpty = snd . mapAccumL go 1  where
    go :: Integer -> Text -> (Integer, Text)
    go n "" = (n + 1, showt n)
    go n s  = (n, s)

-- | Make each string in a list unique (ignoring case) by appending numbers.
uniqueStrings :: [Text] -> [Text]
uniqueStrings ss = snd . mapAccumL (go 1) Set.empty $ ss  where
    go :: Integer -> Set.Set Text -> Text -> (Set.Set Text, Text)
    go n used s | normalize sn `Set.member` used = go (n + 1) used' s
                | otherwise = (used', sn)
      where
        sn | n > 1 || Map.lookup (normalize s) ssOccurrences > Just 1 = s <> (showt n)
           | -- Attempt to only append "1" if there are more occurrences down the list.
             otherwise = s
        used' = Set.insert (normalize sn) used
    normalize     = T.toLower
    ssOccurrences = Map.fromListWith (+) [ (normalize s, 1) | s <- ss ]
