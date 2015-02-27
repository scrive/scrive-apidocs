{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Doc.API.V1.DocumentToJSON (
      evidenceAttachmentsJSONV1
    , documentJSONV1
    , docForListJSONV1
    , docForListCSVV1
    , docForListCSVHeaderV1
  ) where

import Control.Applicative ((<$>))
import Control.Monad.Catch
import Control.Monad.Reader
import Data.List (intercalate)
import Data.List (sortBy, nub)
import Data.Maybe
import Data.String.Utils (strip)
import Text.JSON
import Text.JSON.Gen hiding (value)
import Text.StringTemplates.Templates
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set
import qualified Text.JSON.Gen as J

import Control.Logic
import DB
import DB.TimeZoneName
import Doc.DocInfo
import Doc.DocStateData
import Doc.DocUtils
import File.File
import File.Model
import KontraLink
import MinutesTime
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.String
import Utils.Prelude
import qualified Amazon as AWS
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import qualified Log

evidenceAttachmentsJSONV1 :: (MonadDB m, MonadThrow m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m) => Document -> m JSValue
evidenceAttachmentsJSONV1 doc = do
    evidenceattachments <- EvidenceAttachments.fetch doc
    runJSONGenT $ do
      J.objects "evidenceattachments" $ for evidenceattachments $ \a -> do
        J.value "name"     $ BSC.unpack $ EvidenceAttachments.name a
        J.value "mimetype" $ BSC.unpack <$> EvidenceAttachments.mimetype a
        J.value "downloadLink" $ show $ LinkEvidenceAttachment (documentid doc) (EvidenceAttachments.name a)

documentJSONV1 :: (MonadDB m, MonadThrow m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m) => (Maybe User) -> Bool -> Bool ->  Maybe SignatoryLink -> Document -> m JSValue
documentJSONV1 muser forapi forauthor msl doc = do
    file <- documentfileM doc
    sealedfile <- documentsealedfileM doc
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    runJSONGenT $ do
      J.value "id" $ show $ documentid doc
      J.value "title" $ documenttitle doc
      J.value "file" $ fmap fileJSON file
      J.value "sealedfile" $ fmap fileJSON sealedfile
      J.value "authorattachments" $ map fileJSON authorattachmentfiles
      J.objects "evidenceattachments" $ ([] :: [JSONGenT m ()])
      J.value "time" $ jsonDate (Just $ documentmtime doc)
      J.value "ctime" $ jsonDate (Just $ documentctime doc)
      J.value "timeouttime" $ jsonDate $ documenttimeouttime doc
      J.value "autoremindtime" $ jsonDate $ documentautoremindtime doc
      J.value "status" $ show $ documentstatus doc
      J.value "state" $ show $ documentstatus doc
      J.objects "signatories" $ map (signatoryJSON forapi forauthor doc msl) (documentsignatorylinks doc)
      J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
      J.value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
                                   [StandardAuthentication] -> "standard"
                                   [ELegAuthentication]     -> "eleg"
                                   [SMSPinAuthentication]   -> "sms_pin"
                                   _                        -> "mixed"
      J.value "delivery" $ case nub (map signatorylinkdeliverymethod (documentsignatorylinks doc)) of
                                   [EmailDelivery]   -> "email"
                                   [PadDelivery]     -> "pad"
                                   [APIDelivery]     -> "api"
                                   [MobileDelivery]  -> "mobile"
                                   [EmailAndMobileDelivery]-> "email_mobile"
                                   _                 -> "mixed"
      J.value "template" $ isTemplate doc
      J.value "daystosign" $ documentdaystosign doc
      J.value "daystoremind" $ documentdaystoremind doc
      J.value "showheader" $ documentshowheader doc
      J.value "showpdfdownload" $ documentshowpdfdownload doc
      J.value "showrejectoption" $ documentshowrejectoption doc
      J.value "showfooter" $ documentshowfooter doc
      J.value "invitationmessage" $ "<p>" ++ escapeString (documentinvitetext doc) ++ "</p>" --V1 requires HTML for custom message
      J.value "confirmationmessage" $  "<p>" ++ escapeString (documentconfirmtext doc) ++ "</p>"  --V1 requires HTML for custom message
      J.value "lang" $  case (getLang doc) of -- We keep some old lang codes for old integrations. We should drop it on new API release
                             LANG_EN -> "gb"
                             LANG_SV -> "sv"
                             l -> codeFromLang l
      J.objects "tags" $ for (Set.toList $ documenttags doc) $ \(DocumentTag n v) -> do
                                    J.value "name"  n
                                    J.value "value" v
      J.value "apicallbackurl" $ documentapicallbackurl doc
      J.value "saved" $ not (documentunsaveddraft doc)
      J.value "deleted" $ fromMaybe False $ documentDeletedForUser doc <$> userid <$> muser
      J.value "reallydeleted" $ fromMaybe False $ documentReallyDeletedForUser doc <$> userid <$>  muser
      when (isJust muser) $
        J.value "canperformsigning" $ userCanPerformSigningAction (userid $ fromJust muser) doc
      J.value "objectversion" $ documentobjectversion doc
      J.value "process" $ "Contract"
      J.value "isviewedbyauthor" $ isSigLinkFor muser (getAuthorSigLink doc)
      when (not $ forapi) $ do
        J.value "canberestarted" $ isAuthor msl && ((documentstatus doc) `elem` [Canceled, Timedout, Rejected])
        J.value "canbeprolonged" $ isAuthor msl && ((documentstatus doc) `elem` [Timedout])
        J.value "canbecanceled" $ (isAuthor msl || fromMaybe False (useriscompanyadmin <$> muser)) && documentstatus doc == Pending
        J.value "canseeallattachments" $ isAuthor msl || fromMaybe False (useriscompanyadmin <$> muser)
      J.value "accesstoken" $ show (documentmagichash doc)
      J.value "timezone" $ toString $ documenttimezonename doc

authenticationJSON :: AuthenticationMethod -> JSValue
authenticationJSON StandardAuthentication = toJSValue "standard"
authenticationJSON ELegAuthentication     = toJSValue "eleg"
authenticationJSON SMSPinAuthentication   = toJSValue "sms_pin"


signatoryJSON :: (MonadDB m, MonadThrow m) => Bool -> Bool -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
signatoryJSON forapi forauthor doc viewer siglink = do
    J.value "id" $ show $ signatorylinkid siglink
    J.value "current" $ isCurrent
    J.value "signorder" $ unSignOrder $ signatorysignorder siglink
    J.value "undeliveredInvitation" $ Undelivered == mailinvitationdeliverystatus siglink || Undelivered == smsinvitationdeliverystatus siglink
    J.value "undeliveredMailInvitation" $ Undelivered == mailinvitationdeliverystatus siglink
    J.value "undeliveredSMSInvitation" $  Undelivered == smsinvitationdeliverystatus siglink
    J.value "deliveredInvitation" $ Delivered == mailinvitationdeliverystatus siglink || Delivered == smsinvitationdeliverystatus siglink
    J.value "delivery" $ signatorylinkdeliverymethod siglink
    J.value "confirmationdelivery" $ signatorylinkconfirmationdeliverymethod siglink
    J.value "signs" $ isSignatory siglink
    J.value "author" $ isAuthor siglink
    J.value "saved" $ isJust . maybesignatory $ siglink
    J.value "datamismatch" (Nothing :: Maybe String)
    J.value "signdate" $ jsonDate $ signtime <$> maybesigninfo siglink
    J.value "seendate" $ jsonDate $ signtime <$> maybeseeninfo siglink
    J.value "readdate" $ jsonDate $ maybereadinvite siglink
    J.value "rejecteddate" $ jsonDate rejectedDate
    J.value "rejectionreason" $ signatorylinkrejectionreason siglink
    J.value "fields" $ signatoryFieldsJSON doc siglink
    J.value "status" $ show $ signatoryStatusClass doc siglink
    J.objects "attachments" $ map signatoryAttachmentJSON (signatoryattachments siglink)
    J.value "csv" $ csvcontents <$> signatorylinkcsvupload siglink
    J.value "inpadqueue"  $ False
    J.value "userid" $ show <$> maybesignatory siglink
    J.value "signsuccessredirect" $ signatorylinksignredirecturl siglink
    J.value "rejectredirect" $ signatorylinkrejectredirecturl siglink
    J.value "authentication" $ authenticationJSON $ signatorylinkauthenticationmethod siglink

    when (not (isPreparation doc) && forauthor && forapi && signatorylinkdeliverymethod siglink == APIDelivery) $ do
        J.value "signlink" $ show $ LinkSignDoc doc siglink
    where
      isCurrent = (signatorylinkid <$> viewer) == (Just $ signatorylinkid siglink) || (forauthor &&  isAuthor siglink)
      rejectedDate = signatorylinkrejectiontime siglink

signatoryAttachmentJSON :: (MonadDB m, MonadThrow m) => SignatoryAttachment -> JSONGenT m ()
signatoryAttachmentJSON sa = do
  mfile <- lift $ case (signatoryattachmentfile sa) of
    Just fid -> fmap Just $ dbQuery $ GetFileByFileID fid
    _ -> return Nothing
  J.value "name" $ signatoryattachmentname sa
  J.value "description" $ signatoryattachmentdescription sa
  J.value "file" $ fileJSON <$> mfile

signatoryFieldsJSON :: Document -> SignatoryLink -> JSValue
signatoryFieldsJSON doc sl = JSArray $
  for orderedFields $ \sf@SignatoryField{sfType, sfValue, sfPlacements, sfShouldBeFilledBySender, sfObligatory} -> do

    case sfType of
      FirstNameFT           -> fieldJSON "standard" "fstname"   (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      LastNameFT            -> fieldJSON "standard" "sndname"   (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      EmailFT               -> fieldJSON "standard" "email"     (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      MobileFT              -> fieldJSON "standard" "mobile"    (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      PersonalNumberFT      -> fieldJSON "standard" "sigpersnr" (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CompanyFT             -> fieldJSON "standard" "sigco"     (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CompanyNumberFT       -> fieldJSON "standard" "sigcompnr"  (sfType, sfValue)
                                  ((not $ sfvNull $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      SignatureFT label     -> fieldJSON "signature" label (sfType, sfValue)
                                  (closedSignatureF sf && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CustomFT label closed -> fieldJSON "custom" label          (sfType, sfValue)
                                  (closed  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CheckboxFT label      -> fieldJSON "checkbox" label (sfType, sfValue)
                                  False
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
  where
    closedSignatureF sf = ((not $ sfvNull $ sfValue sf) && (null $ sfPlacements sf) && ((PadDelivery /= signatorylinkdeliverymethod sl)))
    orderedFields = sortBy (\f1 f2 -> ftOrder (sfType f1) (sfType f2)) (signatoryfields sl)
    ftOrder FirstNameFT _ = LT
    ftOrder LastNameFT _ = LT
    ftOrder EmailFT _ = LT
    ftOrder MobileFT _ = LT
    ftOrder CompanyFT _ = LT
    ftOrder PersonalNumberFT _ = LT
    ftOrder CompanyNumberFT _ = LT
    ftOrder _ _ = EQ

fieldJSON :: String -> String -> (FieldType, SignatoryFieldValue) -> Bool -> Bool -> Bool -> [FieldPlacement] -> JSValue
fieldJSON tp name (ft, v) closed obligatory filledbysender placements = runJSONGen $ do
    J.value "type" tp
    J.value "name" name
    J.value "value" $ sfvEncode ft v
    J.value "closed" closed
    J.value "obligatory" obligatory
    J.value "shouldbefilledbysender" filledbysender
    J.value "placements" $ map placementJSON placements


placementJSON :: FieldPlacement -> JSValue
placementJSON placement = runJSONGen $ do
    J.value "xrel" $ placementxrel placement
    J.value "yrel" $ placementyrel placement
    J.value "wrel" $ placementwrel placement
    J.value "hrel" $ placementhrel placement
    J.value "fsrel" $ placementfsrel placement
    J.value "page" $ placementpage placement
    when (not (null (placementanchors placement))) $ do
      J.value "anchors" $ placementanchors placement
    J.value "tip" $ case (placementtipside placement) of
                         Just LeftTip -> Just "left"
                         Just RightTip -> Just "right"
                         _ -> Nothing

instance ToJSValue PlacementAnchor where
  toJSValue anchor = runJSONGen $ do
    J.value "text" (placementanchortext anchor)
    when (placementanchorindex anchor /=1 ) $ do
      J.value "index" (placementanchorindex anchor)
    J.value "pages" (placementanchorpages anchor)

instance ToJSValue ConfirmationDeliveryMethod where
  toJSValue EmailConfirmationDelivery  = toJSValue "email"
  toJSValue MobileConfirmationDelivery = toJSValue "mobile"
  toJSValue EmailAndMobileConfirmationDelivery = toJSValue "email_mobile"
  toJSValue NoConfirmationDelivery = toJSValue "none"

instance ToJSValue DeliveryMethod where
  toJSValue EmailDelivery  = toJSValue "email"
  toJSValue PadDelivery    = toJSValue "pad"
  toJSValue APIDelivery    = toJSValue "api"
  toJSValue MobileDelivery = toJSValue "mobile"
  toJSValue EmailAndMobileDelivery = toJSValue "email_mobile"

jsonDate :: Maybe UTCTime -> JSValue
jsonDate mdate = toJSValue $ formatTimeISO <$> mdate

fileJSON :: File -> JSValue
fileJSON file = runJSONGen $ do
    J.value "id" $ show $ fileid file
    J.value "name" $ filename file


-- Converting document to JSON/CSV for lists
docForListJSONV1 :: TemplatesMonad m => User ->  Document -> m JSValue
docForListJSONV1 user doc = do
  let link = case getSigLinkFor user doc of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  runJSONGenT $ do
    J.object "fields" $ docFieldsListForJSON (userid user) doc
    J.objects "subfields" $ map (signatoryFieldsListForJSON doc) (filter sigFilter (documentsignatorylinks doc))
    J.value "link" $ show link
    J.value "isauthor" $ fromMaybe False (isAuthor <$> getSigLinkFor user doc)
    J.value "docauthorcompanysameasuser" $ Just (usercompany user) == documentauthorcompanyid doc

docFieldsListForJSON :: TemplatesMonad m => UserID -> Document -> JSONGenT m ()
docFieldsListForJSON userid doc = do
    J.value "id" $ show $ documentid doc
    J.value "title" $ documenttitle doc
    J.value "status" $ show $ documentstatusclass doc
    J.value "state"  $ show $ documentstatus doc
    signingParties <- lift $ mapM getSmartNameOrPlaceholder $ getSignatoryPartnerLinks doc
    J.value "party" $ intercalate ", " signingParties
    J.value "partner" $ intercalate ", " $ map getSmartName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "partnercomp" $ intercalate ", " $ map getCompanyName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "author" $ intercalate ", " $ map getSmartName $ filter isAuthor $ (documentsignatorylinks doc)
    J.value "time" $ formatTimeISO (documentmtime doc)
    J.value "ctime" $ formatTimeISO (documentctime doc)
    J.value "timeouttime" $ formatTimeISO <$> documenttimeouttime doc
    J.value "template" $ isTemplate doc
    J.value "partiescount" $ length $ (documentsignatorylinks doc)
    J.value "type" $ case documenttype doc of
                        Template -> "template"
                        Signable -> "signable"
    J.value "process" $ "contract" -- Constant. Need to leave it till we will change API version
    J.value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
      [StandardAuthentication] -> "standard"
      [ELegAuthentication]     -> "eleg"
      [SMSPinAuthentication]   -> "sms_pin"
      _                        -> "mixed"
    J.value "delivery" $ case nub (map signatorylinkdeliverymethod (documentsignatorylinks doc)) of
      [EmailDelivery] -> "email"
      [PadDelivery]   -> "pad"
      [APIDelivery]   -> "api"
      [MobileDelivery]-> "mobile"
      _                        -> "mixed"
    J.value "deliveryMethods" $ nub [ signatorylinkdeliverymethod s | s <- documentsignatorylinks doc
                                    , not (isLastViewer doc s)
                                    ]
    J.value "anyinvitationundelivered" $ Pending == documentstatus doc &&
                                            (   (any (== Undelivered) $ mailinvitationdeliverystatus <$> documentsignatorylinks doc)
                                             || (any (== Undelivered) $ smsinvitationdeliverystatus  <$> documentsignatorylinks doc)
                                            )
    J.value "shared" $ documentsharing doc == Shared
    J.value "file" $ show <$> (documentsealedfile doc `mplus` documentfile doc)
    J.value "inpadqueue" $ False
    J.value "deleted" $ documentDeletedForUser doc userid
    J.value "reallydeleted" $ documentReallyDeletedForUser doc userid
    J.value "canperformsigning" $ userCanPerformSigningAction userid doc
    J.value "isviewedbyauthor" $ isSigLinkFor userid (getAuthorSigLink doc)
    J.value "objectversion" $ documentobjectversion doc

signatoryFieldsListForJSON :: TemplatesMonad m => Document -> SignatoryLink -> JSONGenT m ()
signatoryFieldsListForJSON doc sl = do
    J.value "id" $ show $ signatorylinkid sl
    J.value "status" $ show $ signatoryStatusClass doc sl
    J.value "name" $ case strip (getCompanyName sl) of
                       "" -> getSmartName sl
                       _  -> getSmartName sl ++ " (" ++ getCompanyName sl ++ ")"
    J.value "time" $ fromMaybe "" $ formatTimeISO <$> (sign `mplus` reject `mplus` seen `mplus` open)
    J.value "invitationundelivered" $ Pending == documentstatus doc && (Undelivered == mailinvitationdeliverystatus sl || Undelivered == smsinvitationdeliverystatus sl)
    J.value "inpadqueue" $  False
    J.value "isauthor" $ isAuthor sl
    J.value "cansignnow" $ canSignatorySignNow doc sl
    J.value "authentication" $ case signatorylinkauthenticationmethod sl of
      StandardAuthentication -> "standard"
      ELegAuthentication     -> "eleg"
      SMSPinAuthentication   -> "sms_pin"

    J.value "delivery" $ signatorylinkdeliverymethod sl
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybeseeninfo sl
        reject = signatorylinkrejectiontime sl
        open = maybereadinvite sl


-- Haskell version of statusClassCaseExpression. We don't sort of filter signatories based on that, so there is no need to compute it in DB
signatoryStatusClass :: Document -> SignatoryLink -> StatusClass
signatoryStatusClass doc sl =
  case (documentstatus doc,maybesigninfo sl,maybeseeninfo sl,maybereadinvite sl,  mailinvitationdeliverystatus sl, smsinvitationdeliverystatus sl) of
    (DocumentError _,_,_,_,_,_) -> SCError
    (Preparation,_,_,_,_,_) -> SCDraft
    (_,Just _,_,_,_,_) -> SCSigned
    (Canceled,_,_,_,_,_) -> SCCancelled
    (Timedout,_,_,_,_,_) -> SCTimedout
    (Rejected,_,_,_,_,_) -> SCRejected
    (_,_,Just _,_,_,_) -> SCOpened
    (_,_,_,Just _,_,_) -> SCRead
    (_,_,_,_,Undelivered,_) -> SCDeliveryProblem
    (_,_,_,_,_,Undelivered) -> SCDeliveryProblem
    (_,_,_,_,Delivered,_) -> SCDelivered
    (_,_,_,_,_,Delivered) -> SCDelivered
    _ -> SCSent

-- Converting document into entries in CSV
docForListCSVV1::  Int -> Document -> [[String]]
docForListCSVV1 agr doc = map (signatoryForListCSV agr doc) $ [Nothing] <| null interestingLinks |> map Just interestingLinks
    where interestingLinks = filter (\x-> isSignatory x && getSmartName x /= "") (documentsignatorylinks doc)



signatoryForListCSV::  Int -> Document -> (Maybe SignatoryLink) -> [String]
signatoryForListCSV _agr doc msl = [
              ("'" ++ show (documentid doc) ++ "'") -- Exel trick
            , documenttitle doc
            , show $ documentstatusclass doc
            , getAuthorName $ doc
            , formatTimeSimple $ (documentctime doc)
            , maybe "" formatTimeSimple $ signtime <$> documentinvitetime doc
            , maybe "" formatTimeSimple $ join $ maybereadinvite <$> msl
            , maybe "" formatTimeSimple $ signtime <$> (join $ maybeseeninfo <$> msl)
            , maybe "" formatTimeSimple $ signtime <$> (join $ maybesigninfo <$> msl)
            , fromMaybe  "" $ getFullName <$> msl
            , fromMaybe  "" $ getEmail <$> msl
            , fromMaybe  "" $ getPersonalNumber <$> msl
            , fromMaybe  "" $ getCompanyName <$> msl
            , fromMaybe  "" $ getCompanyNumber <$> msl
            ] ++ (map fieldValue $ sortBy fieldNameSort customFieldsOrCheckbox)
    where
        customFieldsOrCheckbox = filter isCustomOrCheckbox  $ concat $ maybeToList $ signatoryfields <$> msl
        fieldNameSort sf1 sf2 = case (sfType sf1, sfType sf2) of
                                  (CustomFT n1 _, CustomFT n2 _) -> compare n1 n2
                                  (CustomFT _ _,_) -> GT
                                  (SignatureFT n1, SignatureFT n2) -> compare n1 n2
                                  (CheckboxFT n1, CheckboxFT n2) -> compare n1 n2
                                  _ -> EQ
        fieldValue sf = case sfType sf of
                             (CustomFT _ _) -> fromMaybe "" . getTextField $ sfValue sf
                             (CheckboxFT name) -> if (sfvNull $ sfValue sf)
                                                     then name ++ " : not checked"
                                                     else name ++ " : checked"
                             _ -> ""
        isCustomOrCheckbox SignatoryField{sfType} = case sfType of
                                            (CustomFT _ _) -> True
                                            (CheckboxFT _) -> True
                                            _ -> False
docForListCSVHeaderV1 :: [String]
docForListCSVHeaderV1 = [
                          "Id"
                        , "Title"
                        , "Status"
                        , "Author"
                        , "Creation"
                        , "Started"
                        , "Party read invitation"
                        , "Party seen document"
                        , "Party signed document"
                        , "Party name"
                        , "Party mail"
                        , "Party personal number"
                        , "Party company name"
                        , "Party company number"
                       ]
