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
       showTemplatesList
       )
       where

import Archive.View

import InputValidation
import KontraLink
import Kontra
import Doc.DocStateData
import Doc.DocState
import User.UserControl
import User.Model
import Util.FlashUtil
import Util.MonadUtils

import qualified AppLogger as Log

import Control.Applicative
import Control.Monad
import Happstack.State

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
    Context { ctxmaybeuser = Just user } <- getContext
    docids <- getCriticalFieldList asValidDocID "doccheck"
    res <- update . ArchiveDocuments user $ map DocumentID docids
    case res of
      Left msg -> do
        Log.debug $ "Failed to delete docs " ++ (show docids) ++ " : " ++ msg
        mzero
      Right _ -> return ()

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

{- |
    Helper function for showing lists of documents.
-}
someArchivePage :: Kontrakcja m => (User -> m String) -> m (Either KontraLink String)
someArchivePage page = checkUserTOSGet $ do
  user <- guardJustM $ ctxmaybeuser <$> getContext
  page user 
