module Archive.View 
       (
         flashMessageAttachmentArchiveDone,
         flashMessageSignableArchiveDone,
         flashMessageTemplateArchiveDone,
         pageAttachmentList,
         pageDocumentsList,
         pageRubbishBinList,
         pageTemplatesList,
         pagePadDeviceArchive,
         docForListJSON
       )
       where

import Doc.DocStateData
import FlashMessage
import KontraLink
import Templates.Templates
import User.Model
import qualified Templates.Fields as F

import Control.Applicative
import Data.Maybe

import MinutesTime
import Control.Logic
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Control.Monad.Reader
import Text.JSON
import Data.List (intercalate)

import Doc.DocUtils
import PadQueue.Model
import Text.JSON.Gen as J


flashMessageSignableArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageSignableArchiveDone = do
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageDocumentArchiveDone"

flashMessageTemplateArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageTemplateArchiveDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageTemplateArchiveDone"

flashMessageAttachmentArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageAttachmentArchiveDone =
  toFlashMsg OperationDone <$> renderTemplate_ "flashMessageAttachmentArchiveDone"

pageDocumentsList :: TemplatesMonad m => User -> m String
pageDocumentsList = pageList' "pageDocumentsList" LinkContracts 

pageTemplatesList :: TemplatesMonad m => User -> m String
pageTemplatesList = pageList' "pageTemplatesList" LinkTemplates 

pageAttachmentList :: TemplatesMonad m =>  User -> m String
pageAttachmentList = pageList' "pageAttachmentList" LinkAttachments 

pageRubbishBinList :: TemplatesMonad m => User ->  m String
pageRubbishBinList = pageList' "pageRubbishBinList" LinkRubbishBin 

pagePadDeviceArchive :: TemplatesMonad m => User ->  m String
pagePadDeviceArchive = pageList' "pagePadDeviceArchive" LinkPadDeviceArchive
    

{- |
    Helper function for list pages
-}
pageList' :: TemplatesMonad m
          => String
          -> KontraLink
          -> User
          -> m String
pageList' templatename currentlink user = renderTemplate templatename $ do
  F.value "canReallyDeleteDocs" $ useriscompanyadmin user || isNothing (usercompany user)
  F.value "currentlink" $ show $ currentlink
  F.value "linkdoclist" $ show $ LinkContracts
  F.value "documentactive" $ (LinkContracts == currentlink)
  F.value "linkofferlist" $ show $ LinkOffers
  F.value "offeractive" $ (LinkOffers == currentlink)
  F.value "linkorderlist" $ show $ LinkOrders
  F.value "orderactive" $ (LinkOrders == currentlink)
  F.value "linktemplatelist" $ show $ LinkTemplates
  F.value "templateactive" $ (LinkTemplates == currentlink)
  F.value "linkattachmentlist" $ show $ LinkAttachments
  F.value "attachmentactive" $ (LinkAttachments == currentlink)
  F.value "linkrubbishbinlist" $ show $ LinkRubbishBin
  F.value "rubbishbinactive" $ (LinkRubbishBin == currentlink)
  F.value "linkpaddevicearchive" $ show LinkPadDeviceArchive
  F.value "paddevicearchiveactive" $ (LinkPadDeviceArchive == currentlink)

docForListJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> User -> PadQueue ->  Document -> m JSValue
docForListJSON tl crtime user padqueue doc = do
  let link = case getSigLinkFor doc user of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  runJSONGenT $ do
    J.object "fields" $ docFieldsListForJSON tl crtime padqueue doc
    J.objects "subfields" $ map (signatoryFieldsListForJSON tl crtime padqueue doc) (filter sigFilter (documentsignatorylinks doc))
    J.value "link" $ show link

docFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime ->  PadQueue -> Document -> JSONGenT m ()
docFieldsListForJSON tl crtime padqueue doc = do
    J.value "id" $ show $ documentid doc
    J.value "title" $ documenttitle doc
    J.value "status" $ show $ documentstatusclass doc
    J.value "party" $ intercalate ", " $ map getSmartName $ getSignatoryPartnerLinks doc
    J.value "partner" $ intercalate ", " $ map getSmartName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "partnercomp" $ intercalate ", " $ map getCompanyName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)
    J.value "author" $ intercalate ", " $ map getSmartName $ filter isAuthor $ (documentsignatorylinks doc)
    J.value "time" $ showDateAbbrev tl crtime (documentmtime doc)
    J.value "type" $ case documenttype doc of
                        Attachment -> "attachment"
                        Template _ -> "template"
                        Signable _ -> "signable"
    case toDocumentProcess (documenttype doc) of
      Nothing       -> return ()
      Just Contract -> J.value "process" "contract"
      Just Offer    -> J.value "process" "offer"
      Just Order    -> J.value "process" "order"
    J.value "anyinvitationundelivered" $ show $ anyInvitationUndelivered  doc && Pending == documentstatus doc
    J.value "shared" $ show $ documentsharing doc == Shared
    J.value "file" $ fromMaybe "" $ show <$> (listToMaybe $ (documentsealedfiles doc) ++ (documentfiles doc))
    J.value "inpadqueue" $ "true" <| (fmap fst padqueue == Just (documentid doc)) |> "false"

signatoryFieldsListForJSON :: TemplatesMonad m => KontraTimeLocale -> MinutesTime -> PadQueue -> Document -> SignatoryLink -> JSONGenT m ()
signatoryFieldsListForJSON tl crtime padqueue doc sl = do
    J.value "id" $ show $ signatorylinkid sl 
    J.value "status" $ show $ signatorylinkstatusclass sl
    J.value "name" $ getSmartName sl
    J.value "time" $ fromMaybe "" $ (showDateAbbrev tl crtime) <$> (sign `mplus` reject `mplus` seen `mplus` open)
    J.value "invitationundelivered" $ show $ isUndelivered sl && Pending == documentstatus doc
    J.value "inpadqueue" $ "true" <| (fmap fst padqueue == Just (documentid doc)) && (fmap snd padqueue == Just (signatorylinkid sl)) |> "false"
    J.value "isauthor" $ "true" <| isAuthor sl |> "false" 
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybesigninfo sl
        reject = case documentrejectioninfo doc of
          Just (rt, slid, _) | slid == signatorylinkid sl -> Just rt
          _                                               -> Nothing
        open = maybereadinvite sl

