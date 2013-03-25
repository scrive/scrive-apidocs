module Doc.DocStateCommon where

import Control.Monad.Base (MonadBase)
import Control.Monad.Trans (MonadIO)
import Data.Maybe
import Doc.DocStateData
import Doc.DocUtils
import Doc.SignatoryLinkID
import File.FileID (FileID)
import qualified Log
import KontraError (internalError)
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
                , invitationdeliverystatus = Unknown
                , signatorysignatureinfo = Nothing
                , signatorylinkdeleted = False
                , signatorylinkreallydeleted = False
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
          sf FirstNameFT
        , sf LastNameFT
        , sf CompanyFT
        , sf PersonalNumberFT
        , sf CompanyNumberFT
        , sf EmailFT
        , sf MobileFT
        ]
  where sf t = SignatoryField t "" True False []

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
replaceSignatoryUser siglink user mcompany=
  let newsl = replaceSignatoryData
                       siglink
                       (getFirstName      user)
                       (getLastName       user)
                       (getEmail          user)
                       (getMobile         user)
                       (getCompanyName    (user,mcompany))
                       (getPersonalNumber user)
                       (getCompanyNumber  (user,mcompany))
                       (map sfValue $ filter isFieldCustom $ signatoryfields $ signatorydetails siglink) in
  newsl { maybesignatory = Just $ userid user }

-- | Extract main file ID from document, assuming it has been set
documentFileID :: (MonadIO m, MonadBase IO m, Log.MonadLog m) => Document -> m FileID
documentFileID doc =
    case documentfile doc of
      Nothing -> do
        Log.error $ "Missing document file in " ++ show (documentid doc)
        internalError
      Just di -> return di

-- | Extract main sealed file ID from document, assuming it has been set
documentSealedFileID :: (MonadIO m, MonadBase IO m, Log.MonadLog m) => Document -> m FileID
documentSealedFileID doc =
    case documentsealedfile doc of
      Nothing -> do
        Log.error $ "Missing document file in " ++ show (documentid doc)
        internalError
      Just di -> return di
