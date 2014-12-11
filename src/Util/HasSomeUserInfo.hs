-----------------------------------------------------------------------------
-- |
-- Module      :  Util.HasSomeUserInfo
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for abstracting away destructuring to get a name, email,
-- and personal number.
-----------------------------------------------------------------------------
module Util.HasSomeUserInfo (
  emailFromSigLink,
  getMailAddress,
  getFullName,
  getSmartName,
  getSmartNameOrPlaceholder,
  getIdentifier,
  getSignatoryIdentifier,
  signatoryInitials,
  HasSomeUserInfo(..)
  ) where


import Control.Applicative ((<$>))
import Data.Char (toLower)
import Data.List (findIndex, mapAccumL)
import Data.Maybe (fromMaybe)
import Data.String.Utils
import Text.StringTemplates.Templates
import qualified Data.Map as Map
import qualified Data.Set as Set

import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Mails.MailsData
import User.Email
import User.Model
import Utils.String

-- | Anything that might have a first name and last name, or personalnumber
class HasSomeUserInfo a where
  getEmail          :: a -> String
  getFirstName      :: a -> String
  getLastName       :: a -> String
  getPersonalNumber :: a -> String
  getMobile         :: a -> String

instance HasSomeUserInfo UserInfo where
  getEmail          = strip . unEmail . useremail
  getFirstName      = userfstname
  getLastName       = usersndname
  getPersonalNumber = userpersonalnumber
  getMobile         = userphone

instance HasSomeUserInfo User where
  getEmail          = strip . unEmail . useremail . userinfo
  getFirstName      = userfstname         . userinfo
  getLastName       = usersndname         . userinfo
  getPersonalNumber = userpersonalnumber  . userinfo
  getMobile         = userphone          . userinfo


instance HasSomeUserInfo SignatoryLink where
  getEmail          = getEmail . signatoryfields
  getFirstName      = getFirstName . signatoryfields
  getLastName       = getLastName . signatoryfields
  getPersonalNumber = getPersonalNumber . signatoryfields
  getMobile         = getMobile . signatoryfields

instance HasSomeUserInfo [SignatoryField] where
  getEmail          = strip . getTextValueOfType EmailFT
  getFirstName      = getTextValueOfType FirstNameFT
  getLastName       = getTextValueOfType LastNameFT
  getPersonalNumber = getTextValueOfType PersonalNumberFT
  getMobile         = getTextValueOfType MobileFT


{- |
   Given a SignatoryLink, returns a tuple containing the name and the email address.

   Useful for sending emails.
   Refactor note: change this to getNameEmailPair, move to Util.HasSomeUserInfo
 -}
emailFromSigLink :: SignatoryLink -> (String, String)
emailFromSigLink sl = (getFullName sl, getEmail sl)

-- | Get the full name (first last)
getFullName :: (HasSomeUserInfo a) => a -> String
getFullName a = strip $ (strip $ getFirstName a) ++ " " ++ (strip $ getLastName  a)


-- | Return the first non-empty of full name, email or mobile number,
-- for use in frontend.  May not be unique.
getSmartName :: (HasSomeUserInfo a) => a -> String
getSmartName a = firstNonEmpty $ [getFullName a, getEmail a, getMobile a]

-- | Uses '_notNamedParty' localized source text with `getSmartNameFromMaybe`.
getSmartNameOrPlaceholder :: (HasSomeUserInfo a, TemplatesMonad m) => a -> m String
getSmartNameOrPlaceholder a = do
    notNamed <- renderTemplate_ "_notNamedParty"
    return $ firstNonEmpty [getSmartName a, notNamed]

-- | Return the full name plus initials that are unique for the current signing process.
getIdentifier :: Document -> SignatoryLink -> String
getIdentifier doc s | null fullName = initials
                    | otherwise     = fullName ++ " " ++ initials
  where fullName = getFullName s
        initials = "(" ++ signatoryInitials doc s ++ ")"

-- | Return the full name plus first non-empty of person number, email
-- or mobile number.  Temporary definition until we have implemented
-- getIdentifer properly for event log.
getIdentifier' :: HasSomeUserInfo a => a -> String
getIdentifier' a | null fullName   = identifier
                | null identifier = fullName
                | otherwise       = fullName ++ " (" ++ identifier ++ ")"
  where fullName = getFullName a
        identifier = firstNonEmpty [getPersonalNumber a, getEmail a, getMobile a]


-- | Return identifier for a signatory or "(Signatory <N>)" if the
-- identifier is empty, where the signatory is the Nth signatory of
-- the document.
getSignatoryIdentifier :: DocumentMonad m => SignatoryLink -> m String
getSignatoryIdentifier sl = do
  let i = getIdentifier' sl
  if null i then
     maybe "(Anonymous)"
           (("(Signatory " ++) . (++ ")") . show . succ) .
            findIndex ((==(signatorylinkid sl)) . signatorylinkid) .
            documentsignatorylinks
     <$> theDocument
   else
     return i

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a = MailAddress {
    fullname = getFullName a
  , email    = getEmail a
  }

-- | Get initials for a signatory that are unique for the document's
-- current signing process. Substitude numbers for empty initials.
-- When more than one signatory has the same initials, append a unique
-- number.
signatoryInitials :: Document -> SignatoryLink -> String
signatoryInitials doc sl = fromMaybe (error "signatoryInitials") $ lookup (signatorylinkid sl) l where
  l = zip (map signatorylinkid osls)
          (uniqueStrings $ enumerateEmpty $ map initials osls)
  initials :: SignatoryLink -> String
  initials = map head . words . getFullName
  osls = filter signatoryispartner sls ++ filter (not . signatoryispartner) sls
  sls = documentsignatorylinks doc

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
