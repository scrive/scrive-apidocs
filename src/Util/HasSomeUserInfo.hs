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
  getIdentifier,
  getSignatoryIdentifier,
  HasSomeUserInfo(..)
  ) where


import Control.Applicative ((<$>))
import Data.List (find)
import Doc.DocStateData
import Doc.DocumentMonad (DocumentMonad, theDocument)
import User.Model
import Mails.MailsData
import Data.String.Utils
import User.Email

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
  getEmail          = strip . getValueOfType EmailFT
  getFirstName      = getValueOfType FirstNameFT
  getLastName       = getValueOfType LastNameFT
  getPersonalNumber = getValueOfType PersonalNumberFT
  getMobile         = getValueOfType MobileFT


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

-- | Return the full name plus first non-empty of person number,
-- email or mobile number.
getIdentifier :: HasSomeUserInfo a => a -> String
getIdentifier a | null fullName   = identifier
                | null identifier = fullName
                | otherwise       = fullName ++ " (" ++ identifier ++ ")"
  where fullName = getFullName a
        identifier = firstNonEmpty [getPersonalNumber a, getEmail a, getMobile a]

-- | Return identifier for a signatory or "(Signatory <N>)" if the
-- identifier is empty, where the signatory is the Nth signatory of
-- the document.
getSignatoryIdentifier :: DocumentMonad m => SignatoryLink -> m String
getSignatoryIdentifier sl = do
  let i = getIdentifier sl
  if null i then
     maybe "(Anonymous)" (("(Signatory " ++) . (++ ")") . show . fst) .
                 find ((==(signatorylinkid sl)) . signatorylinkid . snd) .
                 zip [1..] . documentsignatorylinks <$> theDocument
   else
     return i

-- | Get a MailAddress
getMailAddress :: (HasSomeUserInfo a) => a -> MailAddress
getMailAddress a = MailAddress {
    fullname = getFullName a
  , email    = getEmail a
  }

-- | Pick first non-empty element or return empty list
firstNonEmpty :: [String] -> String
firstNonEmpty = f . map strip where
  f [a]                = a
  f (a:as) | null a    = firstNonEmpty as
           | otherwise = a
  f []                 = error "firstNonEmpty"
