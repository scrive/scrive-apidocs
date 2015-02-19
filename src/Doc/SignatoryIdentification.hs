module Doc.SignatoryIdentification
  ( SignatoryIdentifierMap
  , SignatoryIdentifier(..)
  , signatoryIdentifierMap
  , signatoryIdentifier
  ) where

import Data.Char (toLower)
import Data.List (mapAccumL)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocStateData (Document, documentsignatorylinks, SignatoryLink, signatorylinkid, signatoryispartner, signatoryisauthor)
import Doc.SignatoryLinkID (SignatoryLinkID)
import Util.HasSomeUserInfo (getFullName)

type SignatoryIdentifierMap = Map SignatoryLinkID SignatoryIdentifier

data SignatoryIdentifier = SignatoryIdentifier
  { siLink     :: Maybe SignatoryLink
  , siFullName :: String
  , siInitials :: String
  }
  deriving Show

-- | Return the full name plus unique initials for a signatory
signatoryIdentifier :: SignatoryIdentifierMap -> SignatoryLinkID -> Maybe String
signatoryIdentifier sim slid = do
  si <- Map.lookup slid sim
  return $ if null (siFullName si) then siInitials si else siFullName si ++ " (" ++ siInitials si ++ ")"

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
    zipWith3 (\slid  name is -> (slid, SignatoryIdentifier (Map.lookup slid slmap) name is))
      (map signatorylinkid sls ++ missing)
      names
      initials
  where
  slmap = Map.fromList [(signatorylinkid s, s) | d <- docs, s <- documentsignatorylinks d]
  names = map getFullName sls ++ map (const "") missing
  initials = uniqueStrings $ enumerateEmpty $ map (map head . words) names
  isLastAuthor | null docs = const False
               | otherwise = \s -> signatoryisauthor s && signatorylinkid s `Set.member` lastdocsigs
    where
      lastdocsigs = Set.fromList (map signatorylinkid (documentsignatorylinks (last docs)))
  sls = filter (\s -> includeviewers || signatoryispartner s
                                     || isLastAuthor s) $ concatMap documentsignatorylinks docs
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
