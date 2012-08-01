module Doc.DocStateCommon where

import Company.Model
import Data.Maybe
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import MagicHash (MagicHash)
import MinutesTime
import Misc
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import qualified Data.Set as S

trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: SignatoryDetails
                     -> [SignatoryRole]
                     -> [SignatoryAttachment]
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' details roles attachments magichash =
  SignatoryLink { signatorylinkid = unsafeSignatoryLinkID 0
                , signatorydetails = signatoryLinkClearDetails details
                , signatorymagichash = magichash
                , maybesignatory = Nothing
                , maybecompany = Nothing
                , maybesigninfo  = Nothing
                , maybeseeninfo  = Nothing
                , maybereadinvite = Nothing
                , invitationdeliverystatus = Unknown
                , signatorysignatureinfo = Nothing
                , signatoryroles = roles
                , signatorylinkdeleted = False
                , signatorylinkreallydeleted = False
                , signatorylinkcsvupload = Nothing
                , signatoryattachments = attachments
                , signatorylinkstatusclass = SCDraft
                }

signatoryLinkClearDetails :: SignatoryDetails -> SignatoryDetails
signatoryLinkClearDetails sd = sd {signatoryfields = map signatoryLinkClearField $ signatoryfields sd}

signatoryLinkClearField :: SignatoryField -> SignatoryField
signatoryLinkClearField field =  case sfType field of
                                      SignatureFT -> field {sfValue = reverse $ dropWhile ((/=) '|' ) $ reverse $ sfValue field}
                                      _ -> field

emptySignatoryFields :: [SignatoryField]
emptySignatoryFields = [
          sf FirstNameFT
        , sf LastNameFT
        , sf CompanyFT
        , sf PersonalNumberFT
        , sf CompanyNumberFT
        , sf EmailFT
        , sf SignatureFT
        ]
    where sf t = SignatoryField t "" []
{- |
    A blank document containing default values that need to be set before
    saving.
-}
blankDocument :: Document
blankDocument =
          Document
          { documentid                   = unsafeDocumentID 0
          , documenttitle                = ""
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentctime                = fromSeconds 0
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = ""
          , documentsealedfiles          = []
          -- , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = S.empty
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          -- , documentattachments          = []
          , documentregion               = defaultValue
          , documentstatusclass          = SCDraft
          }

checkResetSignatoryData :: Document -> [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)] -> [String]
checkResetSignatoryData doc sigs =
  let authors    = [ r | (_, r, _, _) <- sigs, SignatoryAuthor `elem` r]
  in catMaybes $
      [ trueOrMessage (documentstatus doc == Preparation) $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      , trueOrMessage (length authors == 1) $ "Should have exactly one author, had " ++ show (length authors)
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
                        -> [String]
                        -> SignatoryLink
replaceSignatoryData siglink@SignatoryLink{signatorydetails} fstname sndname email company personalnumber companynumber fieldvalues =
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
      CustomFT label _ -> sf { sfType = CustomFT label (not $ null v), sfValue = v }
      CheckboxOptionalFT   _ -> sf
      CheckboxObligatoryFT _ -> sf
      SignatureFT      -> sf)
        : pumpData rest vs'
      where
        (v, vs') = case sfType sf of
          CustomFT{} -> if null vs
                           then ("", [])
                           else (head vs, tail vs)
          _          -> (error "you can't use it", vs)

{- |
    Creates a signable document from a template document.
    The new signable will have the same process as the template,
    and it will be in preparation mode.  It won't be shared.
-}
templateToDocument :: Document -> Document
templateToDocument doc =
    let Template process = documenttype doc in
    doc {
          documentstatus = Preparation
        , documenttype =  Signable process
        , documentsharing = Private
    }



{- |
    Replaces signatory data with given user's data.
-}
replaceSignatoryUser :: SignatoryLink
                        -> User
                        -> Maybe Company
                        -> SignatoryLink
replaceSignatoryUser siglink user mcompany =
  let newsl = replaceSignatoryData
                       siglink
                       (getFirstName      user)
                       (getLastName       user)
                       (getEmail          user)
                       (getCompanyName    mcompany)
                       (getPersonalNumber user)
                       (getCompanyNumber  mcompany)
                       (map sfValue $ filter isFieldCustom $ signatoryfields $ signatorydetails siglink) in
  newsl { maybesignatory = Just $ userid user,
          maybecompany = usercompany user }

checkUpdateFields :: Document -> SignatoryLinkID -> [String]
checkUpdateFields doc slid = catMaybes $
  [ trueOrMessage (isPending doc) $ "Document is not in Pending (is " ++ (show $ documentstatus doc) ++ ")"
  , trueOrMessage (isJust $ getSigLinkFor doc slid) $ "Signatory does not exist"
  , trueOrMessage (not $ hasSigned (doc, slid)) "Signatory has already signed."
  ]

checkAddEvidence :: Document -> SignatoryLinkID -> [String]
checkAddEvidence doc slid = catMaybes $
  [ trueOrMessage (isPending doc) "Document is not in pending"
  , trueOrMessage (isSignatory (doc, slid)) "Given signatorylinkid is not a signatory"
  ]
