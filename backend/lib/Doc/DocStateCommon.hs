module Doc.DocStateCommon where

import Data.Default

import Doc.DocStateData
import Doc.SignatoryFieldID
import Doc.SignatoryLinkID
import MagicHash (MagicHash)
import User.Model
import UserGroup.Data
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: [SignatoryField]
                     -> Bool
                     -> SignatoryRole
                     -> SignOrder
                     -> [SignatoryAttachment]
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' fields author role sorder attachments magichash =
  def { signatorylinkid                         = unsafeSignatoryLinkID 0
      , signatoryfields                         = map signatoryLinkClearField
                                                  fields -- clean signatures
      , signatoryisauthor                       = author
      , signatoryrole                           = role
      , signatorysignorder                      = sorder
      , signatorymagichash                      = magichash
      , maybesignatory                          = Nothing
      , maybesigninfo                           = Nothing
      , maybeseeninfo                           = Nothing
      , maybereadinvite                         = Nothing
      , mailinvitationdeliverystatus            = Unknown
      , smsinvitationdeliverystatus             = Unknown
      , signatorylinkdeleted                    = Nothing
      , signatorylinkreallydeleted              = Nothing
      , signatorylinkcsvupload                  = Nothing
      , signatoryattachments                    = attachments
      , signatorylinksignredirecturl            = Nothing
      , signatorylinkrejectiontime              = Nothing
      , signatorylinkrejectionreason            = Nothing
      , signatorylinkauthenticationtosignmethod = StandardAuthenticationToSign
      }

signatoryLinkClearField :: SignatoryField -> SignatoryField
signatoryLinkClearField field =  case field of
  SignatorySignatureField (sf@(SignatureField{})) -> SignatorySignatureField $ sf { ssfValue = Nothing }
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
      , sefEditableBySignatory    = False
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
      SignatoryNameField _ -> unexpectedError "Name field with order different than 1 or 2"
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
                     -> UserGroup
                     -> SignatoryLink
replaceSignatoryUser siglink user ug =
  (replaceSignatoryData
     siglink
     (getFirstName      user)
     (getLastName       user)
     (getEmail          user)
     (getMobile         user)
     (getCompanyName    ug)
     (getPersonalNumber user)
     (getCompanyNumber  ug)
  ) { maybesignatory = Just $ userid user }
