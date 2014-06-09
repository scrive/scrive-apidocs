module Doc.DocStateCommon where

import Data.Maybe
import Doc.DocStateData
import Doc.DocUtils
import Doc.SignatoryLinkID
import Doc.SignatoryFieldID
import MagicHash (MagicHash)
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Company.Model
import Utils.Default
import Util.SignatoryLinkUtils

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
                , signatoryfields = map signatoryLinkClearField fields -- | Clean signatures
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
                , signatorysignatureinfo = Nothing
                , signatorylinkdeleted = Nothing
                , signatorylinkreallydeleted = Nothing
                , signatorylinkcsvupload = Nothing
                , signatoryattachments = attachments
                , signatorylinkstatusclass = SCDraft
                , signatorylinksignredirecturl = Nothing
                , signatorylinkrejectiontime = Nothing
                , signatorylinkrejectionreason = Nothing
                , signatorylinkauthenticationmethod = StandardAuthentication
                , signatorylinkelegdatamismatchmessage = Nothing
                , signatorylinkelegdatamismatchfirstname = Nothing
                , signatorylinkelegdatamismatchlastname = Nothing
                , signatorylinkelegdatamismatchpersonalnumber = Nothing
                }

signatoryLinkClearField :: SignatoryField -> SignatoryField
signatoryLinkClearField field =  case sfType field of
                                      SignatureFT _ -> field {sfValue = reverse $ dropWhile ((/=) '|' ) $ reverse $ sfValue field}
                                      _ -> field

emptySignatoryFields :: [SignatoryField]
emptySignatoryFields = [
          SignatoryField (unsafeSignatoryFieldID 0) FirstNameFT "" True  True  []
        , SignatoryField (unsafeSignatoryFieldID 0) LastNameFT  "" True  True  []
        , SignatoryField (unsafeSignatoryFieldID 0) EmailFT     "" True  True  []
        ]

checkResetSignatoryData :: Document -> [SignatoryLink] -> [String]
checkResetSignatoryData doc sigs =
  catMaybes $
      [ trueOrMessage (documentstatus doc == Preparation) $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      , trueOrMessage (1 == (length $ filter isAuthor sigs)) $ "Should have exactly one author, had " ++ show (length $ filter isAuthor sigs)
      ]

{- |
    Pumps data into a signatory link
-}
replaceSignatoryData :: SignatoryLink
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> [String]
                        -> SignatoryLink
replaceSignatoryData siglink fstname sndname email mobile company personalnumber companynumber fieldvalues =
  siglink { signatoryfields = pumpData (signatoryfields siglink) fieldvalues }
  where
    pumpData [] _ = []
    pumpData (sf:rest) vs = (case sfType sf of
      FirstNameFT      -> sf { sfValue = fstname }
      LastNameFT       -> sf { sfValue = sndname }
      CompanyFT        -> sf { sfValue = company }
      PersonalNumberFT -> sf { sfValue = personalnumber }
      CompanyNumberFT  -> sf { sfValue = companynumber }
      EmailFT          -> sf { sfValue = email }
      MobileFT         -> sf { sfValue = mobile }
      CustomFT label _ -> sf { sfType = CustomFT label (not $ null v), sfValue = v }
      CheckboxFT _     -> sf
      SignatureFT _    -> sf)
        : pumpData rest vs'
      where
        (v, vs') = case sfType sf of
          CustomFT{} -> if null vs
                           then ("", [])
                           else (head vs, tail vs)
          _          -> (error "you can't use it", vs)


{- |
    Replaces signatory data with given user's data.
-}
replaceSignatoryUser :: SignatoryLink
                     -> User
                     -> Company
                     -> SignatoryLink
replaceSignatoryUser siglink user company=
  let newsl = replaceSignatoryData
                       siglink
                       (getFirstName      user)
                       (getLastName       user)
                       (getEmail          user)
                       (getMobile         user)
                       (getCompanyName    company)
                       (getPersonalNumber user)
                       (getCompanyNumber  company)
                       (map sfValue $ filter isFieldCustom $ signatoryfields $ siglink) in
  newsl { maybesignatory = Just $ userid user }
