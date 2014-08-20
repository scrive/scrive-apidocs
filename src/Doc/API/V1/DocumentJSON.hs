{-# LANGUAGE ExtendedDefaultRules #-}
module Doc.API.V1.DocumentJSON (
    documentJSONV1
  ) where

import Doc.DocStateData
import Doc.DocUtils
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import KontraLink
import MinutesTime
import Utils.Prelude
import Util.SignatoryLinkUtils
import User.Model
import Doc.DocInfo
import Control.Applicative ((<$>))
import Control.Monad.Reader
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe
import Text.JSON
import Data.List (sortBy, nub)
import File.Model
import File.File
import DB
import DB.TimeZoneName
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import qualified Data.Set as Set
import qualified Amazon as AWS
import qualified Log

documentJSONV1 :: (MonadDB m, Log.MonadLog m, MonadIO m, AWS.AmazonMonad m) => (Maybe User) -> Bool -> Bool -> Bool ->  Maybe SignatoryLink -> Document -> m JSValue
documentJSONV1 muser includeEvidenceAttachments forapi forauthor msl doc = do
    file <- documentfileM doc
    sealedfile <- documentsealedfileM doc
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments doc)
    evidenceattachments <- if includeEvidenceAttachments then EvidenceAttachments.fetch doc else return []
    runJSONGenT $ do
      J.value "id" $ show $ documentid doc
      J.value "title" $ documenttitle doc
      J.value "file" $ fmap fileJSON file
      J.value "sealedfile" $ fmap fileJSON sealedfile
      J.value "authorattachments" $ map fileJSON authorattachmentfiles
      J.objects "evidenceattachments" $ for evidenceattachments $ \a -> do
        J.value "name"     $ BSC.unpack $ EvidenceAttachments.name a
        J.value "mimetype" $ BSC.unpack <$> EvidenceAttachments.mimetype a
        J.value "downloadLink" $ show $ LinkEvidenceAttachment (documentid doc) (EvidenceAttachments.name a)
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
      J.value "invitationmessage" $ documentinvitetext doc
      J.value "confirmationmessage" $ documentconfirmtext doc
      J.value "lang" $  case (getLang doc) of -- We keep some old lang codes for old integrations. We should drop it on new API release
                             LANG_EN -> "gb"
                             LANG_SV -> "sv"
                             l -> codeFromLang l
      J.objects "tags" $ for (Set.toList $ documenttags doc) $ \(DocumentTag n v) -> do
                                    J.value "name"  n
                                    J.value "value" v
      J.value "apicallbackurl" $ documentapicallbackurl doc
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


signatoryJSON :: (MonadDB m) => Bool -> Bool -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
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
    J.value "datamismatch" $ signatorylinkelegdatamismatchmessage siglink
    J.value "signdate" $ jsonDate $ signtime <$> maybesigninfo siglink
    J.value "seendate" $ jsonDate $ signtime <$> maybeseeninfo siglink
    J.value "readdate" $ jsonDate $ maybereadinvite siglink
    J.value "rejecteddate" $ jsonDate rejectedDate
    J.value "rejectionreason" $ signatorylinkrejectionreason siglink
    J.value "fields" $ signatoryFieldsJSON doc siglink
    J.value "status" $ show $ signatorylinkstatusclass siglink
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

signatoryAttachmentJSON :: MonadDB m => SignatoryAttachment -> JSONGenT m ()
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
      FirstNameFT           -> fieldJSON "standard" "fstname"   sfValue
                                  ((not $ null $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      LastNameFT            -> fieldJSON "standard" "sndname"   sfValue
                                  ((not $ null $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      EmailFT               -> fieldJSON "standard" "email"     sfValue
                                  ((not $ null $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      MobileFT              -> fieldJSON "standard" "mobile"    sfValue
                                  ((not $ null $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      PersonalNumberFT      -> fieldJSON "standard" "sigpersnr" sfValue
                                  ((not $ null $ sfValue)  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CompanyFT             -> fieldJSON "standard" "sigco"     sfValue
                                  ((not $ null $ sfValue)  && (null sfPlacements) &&
                                      (ELegAuthentication /= signatorylinkauthenticationmethod sl) &&
                                      (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CompanyNumberFT       -> fieldJSON "standard" "sigcompnr"  sfValue
                                  (closedF sf && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      SignatureFT label     -> fieldJSON "signature" label sfValue
                                  (closedSignatureF sf && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CustomFT label closed -> fieldJSON "custom" label          sfValue
                                  (closed  && (not $ isPreparation doc))
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
      CheckboxFT label      -> fieldJSON "checkbox" label sfValue
                                  False
                                  sfObligatory sfShouldBeFilledBySender sfPlacements
  where
    closedF sf = ((not $ null $ sfValue sf) || (null $ sfPlacements sf))
    closedSignatureF sf = ((not $ null $ dropWhile (/= ',') $ sfValue sf) && (null $ sfPlacements sf) && ((PadDelivery /= signatorylinkdeliverymethod sl)))
    orderedFields = sortBy (\f1 f2 -> ftOrder (sfType f1) (sfType f2)) (signatoryfields sl)
    ftOrder FirstNameFT _ = LT
    ftOrder LastNameFT _ = LT
    ftOrder EmailFT _ = LT
    ftOrder MobileFT _ = LT
    ftOrder CompanyFT _ = LT
    ftOrder PersonalNumberFT _ = LT
    ftOrder CompanyNumberFT _ = LT
    ftOrder _ _ = EQ

fieldJSON :: String -> String -> String -> Bool -> Bool -> Bool -> [FieldPlacement] -> JSValue
fieldJSON tp name value closed obligatory filledbysender placements = runJSONGen $ do
    J.value "type" tp
    J.value "name" name
    J.value "value" value
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

jsonDate :: Maybe MinutesTime -> JSValue
jsonDate mdate = toJSValue $ formatMinutesTimeRealISO <$> mdate

fileJSON :: File -> JSValue
fileJSON file = runJSONGen $ do
    J.value "id" $ show $ fileid file
    J.value "name" $ filename file
