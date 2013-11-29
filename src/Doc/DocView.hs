{-# LANGUAGE ExtendedDefaultRules #-}
module Doc.DocView (
    pageCreateFromTemplate
  , documentInfoFields
  , mailDocumentAwaitingForAuthor
  , mailDocumentClosed
  , mailDocumentRejected
  , mailDocumentRemind
  , mailInvitation
  , pageDocumentDesign
  , pageDocumentView
  , pageDocumentSignView
  , documentJSON
  , gtVerificationPage
  ) where

import AppView (kontrakcja, standardPageFields, brandingFields, companyUIForPage, renderFromBody)
import Doc.DocStateData
import Doc.DocUtils
import Doc.DocViewMail
import qualified Doc.EvidenceAttachments as EvidenceAttachments
import Kontra
import KontraLink
import MinutesTime
import Utils.Prelude
import Text.StringTemplates.Templates
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
import PadQueue.Model
import Text.JSON.Gen hiding (value)
import qualified Text.JSON.Gen as J
import qualified Text.StringTemplates.Fields as F
import qualified Data.Set as Set
import Analytics.Include
import qualified Amazon as AWS
import Happstack.Server.SimpleHTTP

pageCreateFromTemplate :: TemplatesMonad m => m String
pageCreateFromTemplate = renderTemplate_ "createFromTemplatePage"

documentJSON :: (MonadDB m, MonadIO m, AWS.AmazonMonad m) => (Maybe User) -> Bool -> Bool -> Bool -> PadQueue -> Maybe SignatoryLink -> Document -> m JSValue
documentJSON muser includeEvidenceAttachments forapi forauthor pq msl doc = do
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
      J.value "status" $ show $ documentstatus doc
      J.value "state" $ show $ documentstatus doc
      J.objects "signatories" $ map (signatoryJSON forapi forauthor pq doc msl) (documentsignatorylinks doc)
      J.value "signorder" $ unSignOrder $ documentcurrentsignorder doc
      J.value "authentication" $ case nub (map signatorylinkauthenticationmethod (documentsignatorylinks doc)) of
                                   [StandardAuthentication] -> "standard"
                                   [ELegAuthentication]     -> "eleg"
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
      J.value "invitationmessage" $ documentinvitetext doc
      J.value "lang" $  case (getLang doc) of
                             LANG_EN -> "gb"
                             LANG_SV -> "sv"
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

authenticationJSON :: AuthenticationMethod -> JSValue
authenticationJSON StandardAuthentication = toJSValue "standard"
authenticationJSON ELegAuthentication     = toJSValue "eleg"

signatoryJSON :: (MonadDB m) => Bool -> Bool -> PadQueue -> Document -> Maybe SignatoryLink -> SignatoryLink -> JSONGenT m ()
signatoryJSON forapi forauthor pq doc viewer siglink = do
    J.value "id" $ show $ signatorylinkid siglink
    J.value "current" $ isCurrent
    J.value "signorder" $ unSignOrder $ signatorysignorder siglink
    J.value "undeliveredInvitation" $ Undelivered == mailinvitationdeliverystatus siglink || Undelivered == smsinvitationdeliverystatus siglink
    J.value "undeliveredMailInvitation" $ Undelivered == mailinvitationdeliverystatus siglink
    J.value "undeliveredSMSInvitation" $  Undelivered == smsinvitationdeliverystatus siglink
    J.value "deliveredInvitation" $ Delivered == mailinvitationdeliverystatus siglink || Delivered == smsinvitationdeliverystatus siglink
    J.value "delivery" $ signatorylinkdeliverymethod siglink
    J.value "signs" $ isSignatory siglink
    J.value "author" $ isAuthor siglink
    J.value "saved" $ isJust . maybesignatory $ siglink
    J.value "datamismatch" $ signatorylinkelegdatamismatchmessage siglink
    J.value "signdate" $ jsonDate $ signtime <$> maybesigninfo siglink
    J.value "seendate" $ jsonDate $ signtime <$> maybeseeninfo siglink
    J.value "readdate" $ jsonDate $ maybereadinvite siglink
    J.value "rejecteddate" $ jsonDate rejectedDate
    J.value "fields" $ signatoryFieldsJSON doc siglink
    J.value "status" $ show $ signatorylinkstatusclass siglink
    J.objects "attachments" $ map signatoryAttachmentJSON (signatoryattachments siglink)
    J.value "csv" $ csvcontents <$> signatorylinkcsvupload siglink
    J.value "inpadqueue"  $ (fmap fst pq == Just (documentid doc)) && (fmap snd pq == Just (signatorylinkid siglink))
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


pageDocumentDesign :: Kontrakcja m
                   => Context
                   -> Document
                   -> AnalyticsData
                   -> m String
pageDocumentDesign ctx document ad = do
     let  mbd = currentBrandedDomain ctx
     mcompany <- companyUIForPage
     renderTemplate "pageDocumentDesign" $ do
         F.value "documentid" $ show $ documentid document
         standardPageFields ctx kontrakcja ad
         brandingFields mbd mcompany

pageDocumentView :: TemplatesMonad m
                    => Document
                    -> Maybe SignatoryLink
                    -> Bool
                    -> m String
pageDocumentView document msiglink authorcompanyadmin =
  renderTemplate "pageDocumentView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ fmap (show . signatorylinkid) msiglink
      F.value "authorcompanyadmin" $ authorcompanyadmin


pageDocumentSignView :: Kontrakcja m
                    => Context
                    -> Document
                    -> SignatoryLink
                    -> AnalyticsData
                    -> m String
pageDocumentSignView ctx document siglink ad = do
  let  mbd = currentBrandedDomain ctx
  mcompany <- companyUIForPage
  renderTemplate "pageDocumentSignView" $ do
      F.value "documentid" $ show $ documentid document
      F.value "siglinkid" $ show $ signatorylinkid siglink
      F.value "documenttitle" $ documenttitle document
      F.value "usestandardheaders" $ (isJust $ maybesignatory siglink) && (maybesignatory siglink) == (userid <$> ctxmaybeuser ctx)
      standardPageFields ctx kontrakcja ad
      brandingFields mbd mcompany



-- | Basic info about document , name, id ,author
documentInfoFields :: Monad m => Document -> Fields m ()
documentInfoFields  document  = do
  F.value "documenttitle" $ documenttitle document
  F.value "title" $ documenttitle document
  F.value "name" $ documenttitle document
  F.value "id" $ show $ documentid document
  F.value "documentid" $ show $ documentid document
  F.value "template" $  isTemplate document
  F.value "hasanyattachments" $ not (null $ (documentauthorattachments document)) || not (null (concatMap signatoryattachments $ documentsignatorylinks document))
  documentStatusFields document

-- | Fields indication what is a document status
documentStatusFields :: Monad m => Document -> Fields m ()
documentStatusFields document = do
  F.value "preparation" $ documentstatus document == Preparation
  F.value "pending" $ documentstatus document == Pending
  F.value "cancel" $ (documentstatus document == Canceled
      && (all (not . isJust . signatorylinkelegdatamismatchmessage) $ documentsignatorylinks document))
  F.value "timedout" $ documentstatus document == Timedout
  F.value "rejected" $ documentstatus document == Rejected
  F.value "signed" $ documentstatus document == Closed
  -- awaitingauthor is used in old view in old template
  -- remove it when old view is removed
  -- currently it means: is the next turn for author to sign?
  F.value "awaitingauthor" $ canAuthorSignNow document
  F.value "datamismatch" $ (documentstatus document == Canceled
      && (any (isJust . signatorylinkelegdatamismatchmessage) $ documentsignatorylinks document))

-- Page for GT verification
gtVerificationPage :: Kontrakcja m => m Response
gtVerificationPage = renderFromBody kontrakcja =<< renderTemplate_ "gtVerificationPage"
