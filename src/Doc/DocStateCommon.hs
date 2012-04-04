
module Doc.DocStateCommon
where

import Company.Model
import Data.Maybe
import Doc.DocInfo
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocUtils
import InputValidation
import MagicHash (MagicHash)
import MinutesTime
import Misc
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils

trueOrMessage :: Bool -> String -> Maybe String
trueOrMessage False s = Just s
trueOrMessage True  _ = Nothing


signLinkFromDetails' :: SignatoryDetails
                     -> [SignatoryRole]
                     -> MagicHash
                     -> SignatoryLink
signLinkFromDetails' details roles magichash =
  SignatoryLink { signatorylinkid = unsafeSignatoryLinkID 0
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
                , signatorylinkcsvupload = Nothing
                , signatoryattachments = []
                , signatorylinkstatusclass = SCDraft
                }


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
          , documentfunctionality        = BasicFunctionality
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
          , documenttags                 = []
          , documentui                   = emptyDocumentUI
          , documentservice              = Nothing
          , documentauthorattachments    = []
          , documentdeleted              = False
          -- , documentattachments          = []
          , documentregion               = defaultValue
          , documentstatusclass          = SCDraft
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

{- |

-}
checkCancelDocument :: Document -> [String]
checkCancelDocument doc = catMaybes $
  [ trueOrMessage (isSignable doc) ("document is not signable")
  , trueOrMessage (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor)
                    ("document should be pending or awaiting author but it is " ++ (show $ documentstatus doc))
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
  , trueOrMessage (all (isSignatory =>>^ (isGood . asValidEmail . getEmail)) (documentsignatorylinks document))
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
  [ trueOrMessage (documentstatus doc == Pending || documentstatus doc == AwaitingAuthor) $ "Document is not in Pending or AwaitingAuthor (is " ++ (show $ documentstatus doc) ++ ")"
  , trueOrMessage (isJust $ getSigLinkFor doc slid) $ "Signatory does not exist"
  , trueOrMessage (not $ hasSigned (doc, slid)) "Signatory has already signed."
  ]

checkAddEvidence :: Document -> SignatoryLinkID -> [String]
checkAddEvidence doc slid = catMaybes $
  [ trueOrMessage (documentstatus doc == Pending) "Document is not in pending"
  , trueOrMessage (isSignatory (doc, slid)) "Given signatorylinkid is not a signatory"
  ]


checkPendingToAwaitingAuthor :: Document -> [String]
checkPendingToAwaitingAuthor doc = catMaybes $
  [ trueOrMessage (documentstatus doc == Pending)
                    ("document should be pending but it is " ++ (show $ documentstatus doc))
  , trueOrMessage (all ((isSignatory &&^ (not . isAuthor)) =>>^ hasSigned) (documentsignatorylinks doc))
                    ("Not all non-author signatories have signed")
  , trueOrMessage (not $ hasSigned $ getAuthorSigLink doc) "Author has already signed"
  ]
