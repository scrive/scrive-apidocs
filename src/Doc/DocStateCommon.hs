module Doc.DocStateCommon where

import Data.Maybe
import qualified Data.ByteString as BS

import Company.Model
import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import MagicHash (MagicHash)
import OurPrelude
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Default

trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: [SignatoryField]
                     -> Bool
                     -> Bool
                     -> SignOrder
                     -> [SignatoryAttachment]
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' fields author partner sorder attachments magichash =
  defaultValue {  signatorylinkid = unsafeSignatoryLinkID 0
                , signatoryfields = map signatoryLinkClearField fields -- clean signatures
                , signatoryisauthor  = author
                , signatoryispartner = partner
                , signatorysignorder  = sorder
                , signatorymagichash = magichash
                , maybesignatory = Nothing
                , maybesigninfo  = Nothing
                , maybeseeninfo  = Nothing
                , maybereadinvite = Nothing
                , mailinvitationdeliverystatus = Unknown
                , smsinvitationdeliverystatus = Unknown
                , signatorylinkdeleted = Nothing
                , signatorylinkreallydeleted = Nothing
                , signatorylinkcsvupload = Nothing
                , signatoryattachments = attachments
                , signatorylinksignredirecturl = Nothing
                , signatorylinkrejectiontime = Nothing
                , signatorylinkrejectionreason = Nothing
                , signatorylinkauthenticationmethod = StandardAuthentication
                }

signatoryLinkClearField :: SignatoryField -> SignatoryField
signatoryLinkClearField field =  case field of
  SignatorySignatureField (sf@(SignatureField{})) -> SignatorySignatureField $ sf { ssfValue = BS.empty }
  _ -> field

emptySignatoryFields :: [SignatoryField]
emptySignatoryFields = [
    SignatoryNameField $ NameField {
        snfID                     = (unsafeSignatoryFieldID 0)
      , snfNameOrder              = NameOrder 1
      , snfValue                  = ""
      , snfObligatory             = True
      , snfShouldBeFilledBySender = True
      , snfPlacements             = []
    }
  , SignatoryNameField $ NameField {
        snfID                     = (unsafeSignatoryFieldID 0)
      , snfNameOrder              = NameOrder 2
      , snfValue                  = ""
      , snfObligatory             = True
      , snfShouldBeFilledBySender = True
      , snfPlacements             = []
    }
  , SignatoryEmailField $ EmailField {
        sefID                     = (unsafeSignatoryFieldID 0)
      , sefValue                  = ""
      , sefObligatory             = True
      , sefShouldBeFilledBySender = True
      , sefPlacements             = []
    }
  ]

checkResetSignatoryData :: Document -> [SignatoryLink] -> [String]
checkResetSignatoryData doc sigs =
  catMaybes $
      [ trueOrMessage (documentstatus doc == Preparation) $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      , trueOrMessage (1 == (length $ filter isAuthor sigs)) $ "Should have exactly one author, had " ++ show (length $ filter isAuthor sigs)
      ]

{- |
    Pumps data into a signatory link
    FIXME: used only in replaceSignatoryUser
-}
replaceSignatoryData :: SignatoryLink
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> SignatoryLink
replaceSignatoryData siglink fstname sndname email mobile company personalnumber companynumber =
  siglink { signatoryfields = map fillData (signatoryfields siglink) }
  where
    fillData sf = case sf of
      SignatoryNameField nf@(NameField {snfNameOrder = NameOrder 1}) -> SignatoryNameField $ nf { snfValue = fstname }
      SignatoryNameField nf@(NameField {snfNameOrder = NameOrder 2}) -> SignatoryNameField $ nf { snfValue = sndname }
      SignatoryNameField _ -> $unexpectedError "Name field with order different than 1 or 2"
      SignatoryCompanyField cf@(CompanyField{}) -> SignatoryCompanyField $ cf { scfValue = company }
      SignatoryPersonalNumberField pnf@(PersonalNumberField{}) -> SignatoryPersonalNumberField $ pnf { spnfValue = personalnumber }
      SignatoryCompanyNumberField cnf@(CompanyNumberField{}) -> SignatoryCompanyNumberField $ cnf { scnfValue = companynumber }
      SignatoryEmailField cef@(EmailField {}) -> SignatoryEmailField $ cef { sefValue = email }
      SignatoryMobileField cmf@(MobileField{}) -> SignatoryMobileField $ cmf { smfValue = mobile }
      _  -> sf

{- |
    Replaces signatory data with given user's data.
-}
replaceSignatoryUser :: SignatoryLink
                     -> User
                     -> Company
                     -> SignatoryLink
replaceSignatoryUser siglink user company=
  (replaceSignatoryData
     siglink
     (getFirstName      user)
     (getLastName       user)
     (getEmail          user)
     (getMobile         user)
     (getCompanyName    company)
     (getPersonalNumber user)
     (getCompanyNumber  company)
  ) { maybesignatory = Just $ userid user }

