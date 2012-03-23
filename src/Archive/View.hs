module Archive.View 
       (
         flashMessageAttachmentArchiveDone,
         flashMessageSignableArchiveDone,
         flashMessageTemplateArchiveDone,
         pageAttachmentList,
         pageContractsList,
         pageOffersList,
         pageOrdersList,
         pageRubbishBinList,
         pageTemplatesList,
         docForListJSON
       )
       where

import Doc.DocProcess
import Doc.DocStateData
import FlashMessage
import KontraLink
import Templates.Templates
import User.Model

import Control.Applicative
import Data.Maybe

import MinutesTime
import Misc
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Doc.JSON()
import Control.Monad.Reader
import Text.JSON
import Data.List (intercalate)

import Doc.DocView
import Util.JSON
import Doc.DocUtils

flashMessageSignableArchiveDone :: TemplatesMonad m => DocumentType -> m FlashMessage
flashMessageSignableArchiveDone doctype = do
  toFlashMsg OperationDone <$> renderTextForProcess doctype processflashmessagearchivedone

flashMessageTemplateArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageTemplateArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageTemplateArchiveDone" ()

flashMessageAttachmentArchiveDone :: TemplatesMonad m => m FlashMessage
flashMessageAttachmentArchiveDone =
  toFlashMsg OperationDone <$> renderTemplateM "flashMessageAttachmentArchiveDone" ()
  

pageContractsList :: TemplatesMonad m => User -> m String
pageContractsList = pageList' "pageContractsList" LinkContracts

pageTemplatesList :: TemplatesMonad m => User -> m String
pageTemplatesList = pageList' "pageTemplatesList" LinkTemplates

pageAttachmentList :: TemplatesMonad m =>  User -> m String
pageAttachmentList = pageList' "pageAttachmentList" LinkAttachments

pageOffersList :: TemplatesMonad m => User -> m String
pageOffersList = pageList' "pageOffersList" LinkOffers

pageOrdersList :: TemplatesMonad m => User -> m String
pageOrdersList = pageList' "pageOrdersList" LinkOrders

pageRubbishBinList :: TemplatesMonad m => User ->  m String
pageRubbishBinList = pageList' "pageRubbishBinList" LinkRubbishBin

{- |
    Helper function for list pages
-}
pageList' :: TemplatesMonad m
          => String
          -> KontraLink
          -> User
          -> m String
pageList' templatename currentlink user  =
  renderTemplateFM templatename $ do
    field "canReallyDeleteDocs" $ useriscompanyadmin user || isNothing (usercompany user)
    field "currentlink" $ show $ currentlink
    field "linkdoclist" $ show $ LinkContracts 
    field "documentactive" $ (LinkContracts == currentlink)
    field "linkofferlist" $ show $ LinkOffers 
    field "offeractive" $ (LinkOffers == currentlink)
    field "linkorderlist" $ show $ LinkOrders
    field "orderactive" $ (LinkOrders == currentlink)
    field "linktemplatelist" $ show $ LinkTemplates
    field "templateactive" $ (LinkTemplates == currentlink)
    field "linkattachmentlist" $ show $ LinkAttachments 
    field "attachmentactive" $ (LinkAttachments == currentlink)
    field "linkrubbishbinlist" $ show $ LinkRubbishBin
    field "rubbishbinactive" $ (LinkRubbishBin == currentlink)


docForListJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> User -> Document -> m (JSObject JSValue)
docForListJSON tl crtime user doc =
  let link = case getSigLinkFor doc user of
        Just sl | not $ isAuthor sl -> LinkSignDoc doc sl
        _                           -> LinkIssueDoc $ documentid doc
      sigFilter sl =   isSignatory sl && (documentstatus doc /= Preparation)
  in fmap toJSObject $ propagateMonad  $
    [ ("fields" , jsonPack <$> docFieldsListForJSON tl crtime doc),
      ("subfields" , JSArray <$>  fmap jsonPack <$> (mapM (signatoryFieldsListForJSON tl crtime doc) (filter sigFilter (documentsignatorylinks doc)))),
      ("link", return $ JSString $ toJSString $  show link)
    ]
    
docFieldsListForJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> Document -> m [(String,String)]
docFieldsListForJSON tl crtime doc =  propagateMonad [
    ("id", return $ show $ documentid doc),
    ("title",return $ documenttitle doc),
    ("status", return $ show $ documentStatusClass doc),
    ("party", return $ intercalate ", " $ map getSmartName $ getSignatoryPartnerLinks doc),
    ("partner", return $ intercalate ", " $ map getSmartName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)),
    ("partnercomp", return $ intercalate ", " $ map getCompanyName $ filter (not . isAuthor) (getSignatoryPartnerLinks doc)),
    ("author", return $ intercalate ", " $ map getSmartName $ filter isAuthor $ (documentsignatorylinks doc)),
    ("time", return $ showDateAbbrev tl crtime (documentmtime doc)),
    ("process", renderTextForProcess doc processname),
    ("type", renderDocType),
    ("anyinvitationundelivered", return $ show $ anyInvitationUndelivered  doc && Pending == documentstatus doc),
    ("shared", return $ show $ (documentsharing doc)==Shared),
    ("file", return $ fromMaybe "" $ show <$> (listToMaybe $ (documentsealedfiles doc) ++ (documentfiles doc)))
    ]
  where
    renderDocType :: (TemplatesMonad m) => m String
    renderDocType = do
      pn <- renderTextForProcess doc processname
      case documenttype doc of
        Attachment -> renderTemplateFM "docListAttachmentLabel" $ do return ()
        AttachmentTemplate -> renderTemplateFM "docListAttachmentLabel" $ do return ()
        Template _ -> renderTemplateFM "docListTemplateLabel" $ do field "processname" pn
        Signable _ -> return pn

signatoryFieldsListForJSON :: (TemplatesMonad m) => KontraTimeLocale -> MinutesTime -> Document ->  SignatoryLink -> m [(String,String)]
signatoryFieldsListForJSON tl crtime doc sl = propagateMonad [
    ("status", return $ show $ signatoryStatusClass doc sl ),
    ("name", return $ getSmartName sl),
    ("time", return $ fromMaybe "" $ (showDateAbbrev tl crtime) <$> (sign `mplus` reject `mplus` seen `mplus` open)),
    ("invitationundelivered", return $ show $ isUndelivered sl && Pending == documentstatus doc)
    ]
    where
        sign = signtime <$> maybesigninfo sl
        seen = signtime <$> maybesigninfo sl
        reject = case documentrejectioninfo doc of
                    Just (rt, slid, _)
                        | slid == signatorylinkid sl -> Just $ rt
                    _                             -> Nothing
        open = maybereadinvite sl

        