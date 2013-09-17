module Doc.DocStateCommon where

import Data.Maybe
import Doc.DocStateData
import Doc.DocUtils
import Doc.SignatoryLinkID
import MagicHash (MagicHash)
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Company.Model
import Utils.Default

trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: SignatoryDetails
                     -> [SignatoryAttachment]
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' details attachments magichash =
  defaultValue { signatorylinkid = unsafeSignatoryLinkID 0
                , signatorydetails = signatoryLinkClearDetails details
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

signatoryLinkClearDetails :: SignatoryDetails -> SignatoryDetails
signatoryLinkClearDetails sd = sd {signatoryfields = map signatoryLinkClearField $ signatoryfields sd}

signatoryLinkClearField :: SignatoryField -> SignatoryField
signatoryLinkClearField field =  case sfType field of
                                      SignatureFT _ -> field {sfValue = reverse $ dropWhile ((/=) '|' ) $ reverse $ sfValue field}
                                      _ -> field

emptySignatoryFields :: [SignatoryField]
emptySignatoryFields = [
          SignatoryField FirstNameFT "" True  True  []
        , SignatoryField LastNameFT  "" True  True  []
        , SignatoryField EmailFT     "" True  True  []
        ]

checkResetSignatoryData :: Document -> [(SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload, Maybe String, AuthenticationMethod, DeliveryMethod)] -> [String]
checkResetSignatoryData doc sigs =
  let authors = length $ filter (\(d, _, _, _, _, _) -> signatoryisauthor d) sigs
  in catMaybes $
      [ trueOrMessage (documentstatus doc == Preparation) $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      , trueOrMessage (authors == 1) $ "Should have exactly one author, had " ++ show authors
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
replaceSignatoryData siglink@SignatoryLink{signatorydetails} fstname sndname email mobile company personalnumber companynumber fieldvalues =
  siglink { signatorydetails = signatorydetails { signatoryfields = pumpData (signatoryfields signatorydetails) fieldvalues } }
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
                       (map sfValue $ filter isFieldCustom $ signatoryfields $ signatorydetails siglink) in
  newsl { maybesignatory = Just $ userid user }
