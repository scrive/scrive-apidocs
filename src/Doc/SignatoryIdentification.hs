module Doc.SignatoryIdentification
  ( SignatoryIdentifierMap
  , SignatoryIdentifier(..)
  , signatoryIdentifierMap
  , signatoryIdentifier
  ) where

import Data.Char (toLower)
import Data.Function (on)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocStateData (Document, documentsignatorylinks, SignatoryLink, signatorylinkid, signatoryispartner, signatoryisauthor)
import Doc.SignatoryLinkID (SignatoryLinkID)
import KontraPrelude
import Util.HasSomeUserInfo (getFullName)

type SignatoryIdentifierMap = Map SignatoryLinkID SignatoryIdentifier

data SignatoryIdentifier = SignatoryIdentifier
  { siLink     :: Maybe SignatoryLink
  , siFullName :: String
  , siInitials :: String
  }
  deriving Show

-- | Return the full name plus unique initials for a signatory
signatoryIdentifier :: SignatoryIdentifierMap -> SignatoryLinkID -> String -> Maybe String
signatoryIdentifier sim slid emptyNamePlaceholder = do
  si <- Map.lookup slid sim
  return $ (if null (siFullName si) then emptyNamePlaceholder else siFullName si) ++ " (" ++ siInitials si ++ ")"

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
signatoryIdentifierMap :: Bool -> [Document] -> Set SignatoryLinkID -> SignatoryIdentifierMap
signatoryIdentifierMap includeviewers docs slids =
  Map.fromList $
    zipWith3 (\esl name is -> (esignatorylinkid esl, SignatoryIdentifier (Map.lookup (esignatorylinkid esl) slmap) name is))
      slsAndMissing
      names
      initials
  where
  esignatorylinkid = either signatorylinkid id
  slmap = Map.fromList [(signatorylinkid s, s) | d <- docs, s <- documentsignatorylinks d]
  names = map (either getFullName (const "")) slsAndMissing
  initials = uniqueStrings $ enumerateEmpty $ map (map $head . words) names
  sls = filter (\s -> includeviewers || signatoryispartner s
                                     || signatoryisauthor s) $ concatMap documentsignatorylinks docs
  slsAndMissing = sortBy (compare `on` esignatorylinkid) $ (map Left sls) ++ (map Right missing)
  missing = Set.toList $
            slids Set.\\ Set.fromList (map signatorylinkid sls)

-- | Replace all empty strings in a list with "1", "2", ...
enumerateEmpty :: [String] -> [String]
enumerateEmpty = snd . mapAccumL go 1 where
  go n "" = (n+1,show n)
  go n s  = (n,s)

-- | Make each string in a list unique (ignoring case) by appending numbers.
uniqueStrings :: [String] -> [String]
uniqueStrings ss = snd . mapAccumL (go 1) Set.empty $ ss where
  go n used s | normalize sn `Set.member` used = go (n+1) used' s
              | otherwise          = (used',sn)
    where sn | n > 1 || Map.lookup (normalize s) ssOccurrences > Just 1 = s ++ show n -- Attempt to only append "1" if there are more occurrences down the list.
             | otherwise                                                = s
          used' = Set.insert (normalize sn) used
  normalize = map toLower
  ssOccurrences = Map.fromListWith (+) [(normalize s,1) | s <- ss]
