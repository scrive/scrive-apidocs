module Archive.Control
       (
       handleAttachmentArchive,
       handleContractArchive,
       handleIssueArchive,
       handleOffersArchive,
       handleOrdersArchive,
       handleSignableArchive,
       handleTemplateArchive,
       showAttachmentList,
       showContractsList,
       showOfferList,
       showOrdersList,
       showRubbishBinList,
       showTemplatesList,
       showPadDeviceArchive
       )
       where

import Archive.View

import InputValidation
import KontraLink
import Kontra
import DB.Classes
import Doc.DocStateData
import Doc.Model
import User.UserControl
import User.Model
import Util.FlashUtil
import Util.MonadUtils

import Control.Applicative
import Util.SignatoryLinkUtils
import Stats.Control

import EvidenceLog.Model
import Util.HasSomeUserInfo

import qualified Data.ByteString.UTF8 as BS

handleContractArchive :: Kontrakcja m => m KontraLink
handleContractArchive = do
    _ <- handleSignableArchive (Signable Contract)
    return $ LinkContracts

handleOffersArchive :: Kontrakcja m => m KontraLink
handleOffersArchive =  do
    _ <- handleSignableArchive (Signable Offer)
    return $ LinkOffers

handleOrdersArchive :: Kontrakcja m => m KontraLink
handleOrdersArchive =  do
    _ <- handleSignableArchive (Signable Order)
    return $ LinkOrders

handleSignableArchive :: Kontrakcja m => DocumentType -> m ()
handleSignableArchive doctype =  do
    handleIssueArchive
    addFlashM $ flashMessageSignableArchiveDone doctype
    return ()

handleTemplateArchive :: Kontrakcja m => m KontraLink
handleTemplateArchive = do
    handleIssueArchive
    addFlashM flashMessageTemplateArchiveDone
    return $ LinkTemplates

handleAttachmentArchive :: Kontrakcja m => m KontraLink
handleAttachmentArchive = do
    handleIssueArchive
    addFlashM flashMessageAttachmentArchiveDone
    return $ LinkAttachments

handleIssueArchive :: Kontrakcja m => m ()
handleIssueArchive = do
    Context { ctxmaybeuser = Just user, ctxtime, ctxipnumber } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    let actor = UserActor ctxtime ctxipnumber (userid user) (BS.toString $ getEmail user)
    mapM_ (\did -> do 
              doc <- guardRightM' $ runDBUpdate $ ArchiveDocument user did actor
              case getSigLinkFor doc user of
                Just sl -> runDB $ addSignStatDeleteEvent doc sl ctxtime
                _ -> return False) 
      docids

{- |
   Constructs a list of documents (Arkiv) to show to the user.
 -}
showContractsList :: Kontrakcja m => m (Either KontraLink String)
showContractsList = someArchivePage pageContractsList

showOfferList :: Kontrakcja m => m (Either KontraLink String)
showOfferList = someArchivePage pageOffersList

showOrdersList :: Kontrakcja m => m (Either KontraLink String)
showOrdersList = someArchivePage pageOrdersList

showTemplatesList :: Kontrakcja m => m (Either KontraLink String)
showTemplatesList = someArchivePage pageTemplatesList

showAttachmentList :: Kontrakcja m => m (Either KontraLink String)
showAttachmentList = someArchivePage pageAttachmentList

showRubbishBinList :: Kontrakcja m => m (Either KontraLink String)
showRubbishBinList = someArchivePage pageRubbishBinList

showPadDeviceArchive :: Kontrakcja m => m (Either KontraLink String)
showPadDeviceArchive = someArchivePage pagePadDeviceArchive

{- |
    Helper function for showing lists of documents.
-}
someArchivePage :: Kontrakcja m => (User -> m String) -> m (Either KontraLink String)
someArchivePage page = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  page user

  