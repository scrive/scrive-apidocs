{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Doc.DocStateCommon
where

--import API.Service.Model
--import Company.Model
--import Control.Monad
--import Control.Monad.Reader (ask)
--import Database.HDBC
--import Data.Maybe
--import Data.Word
--import DB.Classes
import DB.Types
--import DB.Utils
import Doc.DocProcess
import Doc.DocStateData
--import Doc.DocStateUtils
import Doc.DocUtils
--import Happstack.Data.IxSet as IxSet hiding (null)
--import Happstack.State
import Mails.MailsUtil
import MinutesTime
import Misc
import User.Model
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS
--import Util.SignatoryLinkUtils
--import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import InputValidation
--import Control.Applicative
import Doc.DocInfo
--import Data.List
--import File.FileID
--import qualified AppLogger as Log
--import qualified Doc.Model as D
--import qualified Doc.Tables as D
import Data.Maybe
import Util.SignatoryLinkUtils

                  
{- |

 -}
trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: SignatoryDetails
                     -> [SignatoryRole]
                     -> SignatoryLinkID
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' details roles linkid magichash = 
  SignatoryLink { signatorylinkid = linkid
                , signatorydetails = details
                , signatorymagichash = magichash
                , maybesignatory = Nothing
                , maybesupervisor = Nothing  -- This field is now deprecated should use maybecompany instead
                , maybecompany = Nothing
                , maybesigninfo  = Nothing
                , maybeseeninfo  = Nothing
                , maybereadinvite = Nothing
                , invitationdeliverystatus = Unknown
                , signatorysignatureinfo = Nothing
                , signatoryroles = roles
                , signatorylinkdeleted = False
                , signatorylinkreallydeleted = False
                }

{- |
    A blank document containing default values that need to be set before
    saving.
-}
blankDocument :: Document
blankDocument =
          Document
          { documentid                   = DocumentID 0
          , documenttitle                = BS.empty
          , documentsignatorylinks       = []
          , documentfiles                = []
          , documentstatus               = Preparation
          , documenttype                 = Signable Contract
          , documentfunctionality        = BasicFunctionality
          , documentctime                = fromSeconds 0 
          , documentmtime                = fromSeconds 0
          , documentdaystosign           = Nothing
          , documenttimeouttime          = Nothing
          , documentlog                  = []
          , documentinvitetext           = BS.empty
          , documentsealedfiles          = []
          -- , documenttrustweaverreference = Nothing
          , documentallowedidtypes       = []
          , documentcsvupload            = Nothing
          , documentcancelationreason    = Nothing
          , documentinvitetime           = Nothing
          , documentsharing              = Private
          , documentrejectioninfo        = Nothing
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          , documentsignatoryattachments = []
          -- , documentattachments          = []
          , documentregion               = defaultValue
          }


{- |
    Determines whether a new document should have either Advanced or Basic
    functionality according to the document's type and the user's preferences.
-}
newDocumentFunctionality :: DocumentType -> User -> DocumentFunctionality
newDocumentFunctionality documenttype user =
  case (getValueForProcess documenttype processbasicavailable, 
        preferreddesignmode $ usersettings user) of
    (Just True, Nothing) -> BasicFunctionality
    (Just True, Just BasicMode) -> BasicFunctionality
    _ -> AdvancedFunctionality

{- |

-}
checkCloseDocument :: Document -> [String]
checkCloseDocument doc = catMaybes $
  [ trueOrMessage (isSignable doc) ("document is not signable")
  , trueOrMessage (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                    ("document should be pending or awaiting author but it is " ++ (show $ documentstatus doc))
  , trueOrMessage (all (isSignatory =>>^ hasSigned) (documentsignatorylinks doc)) 
                    ("Not all signatories have signed")
  ]

{- | Preconditions for moving a document from Preparation to Pending.
 -}
checkPreparationToPending :: Document -> [String]
checkPreparationToPending document = catMaybes $
  [ trueOrMessage (isSignable document) ("document is not signable")
  , trueOrMessage (documentstatus document == Preparation)
                    ("Document status is not pending (is " ++ (show . documentstatus) document ++ ")")
  , trueOrMessage (length (filter isAuthor $ documentsignatorylinks document) == 1)
                    ("Number of authors was not 1")
  , trueOrMessage (length (filter isSignatory $ documentsignatorylinks document) >= 1)
                    ("There are no signatories")
  , trueOrMessage (all (isSignatory =>>^ (isGood . asValidEmail . BS.toString . getEmail)) (documentsignatorylinks document))
                    ("Not all signatories have valid email")
  , trueOrMessage (length (documentfiles document) == 1) "Did not have exactly one file"
  ]
  -- NOTE: Should add stuff about first/last name, though currently the author may have his full name
  -- stored in the first name field. OOPS!


-- FIXME: check magic hash token
-- FIXME: check proper role
checkRejectDocument :: Document -> SignatoryLinkID -> [String]
checkRejectDocument doc slid = catMaybes $
  [ trueOrMessage (isSignable doc) ("document is not signable")
  , trueOrMessage (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                    ("document should be pending or awaiting author but it is " ++ (show $ documentstatus doc))
  , trueOrMessage (any ((== slid) . signatorylinkid) (documentsignatorylinks doc))
                  ("signatory #" ++ show slid ++ " is not in the list of document signatories")
  ]

checkSignDocument :: Document -> SignatoryLinkID -> MagicHash -> [String]
checkSignDocument doc slid mh = catMaybes $
  [ trueOrMessage (isPending doc || isAwaitingAuthor doc) "Document is not in pending"
  , trueOrMessage (not $ hasSigned (doc, slid)) "Signatory has already signed"
  , trueOrMessage (hasSeen (doc, slid)) "Signatory has not seen"
  , trueOrMessage (isJust $ getSigLinkFor doc slid) "Signatory does not exist"
  , trueOrMessage (validSigLink slid mh (Just doc)) "Magic Hash does not match"
  ]

checkResetSignatoryData :: Document -> [(SignatoryDetails, [SignatoryRole])] -> [String]
checkResetSignatoryData doc sigs = 
  let authors    = [ r | (_, r) <- sigs, SignatoryAuthor `elem` r]
      nonauthors = [ r | (_, r) <- sigs, SignatoryAuthor `notElem` r]
      isbasic = documentfunctionality doc == BasicFunctionality
      disallowspartner _ = getValueForProcess doc processauthorsend /= Just True
  in catMaybes $
      [ trueOrMessage (documentstatus doc == Preparation) $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      , trueOrMessage (length authors == 1) $ "Should have exactly one author, had " ++ show (length authors)
      , trueOrMessage (isbasic =>> (length nonauthors <= 1)) $ "Should be at most one signatory since it's basic functionality"
      , trueOrMessage (isbasic =>> all (disallowspartner =>>^ (SignatoryPartner `notElem`)) authors) "The author should not be a signatory with this doc type and basic functionality"
      , trueOrMessage (isbasic =>> none (hasFieldsAndPlacements . fst) sigs) "The signatories should have no custom fields or placements" 
      ]

{- |
    Pumps data into a signatory link
-}
replaceSignatoryData :: SignatoryLink
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> BS.ByteString
                        -> [BS.ByteString]
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
      CustomFT label _ -> sf { sfType = CustomFT label (not $ BS.null v), sfValue = v })
        : pumpData rest vs'
      where
        (v, vs') = case sfType sf of
          CustomFT{} -> if null vs
                           then (BS.empty, [])
                           else (head vs, tail vs)
          _          -> (error "you can't use it", vs)
