module Doc.DocViewMail (
      mailCancelDocument
    , mailCancelDocumentContent
    , mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentErrorForAuthor
    , mailDocumentErrorForSignatory
    , mailDocumentRejected
    , mailDocumentRemind
    , mailDocumentRemindContent
    , InvitationTo(..)
    , mailInvitation
    , mailInvitationContent
    , mailMismatchAuthor
    , mailMismatchSignatory
    , mailRejectMailContent
    , documentMailWithDocLocale
    , mailFooterForInviter
    , mailFooterForInvitee
    , mailFooterForService
    ) where

import API.Service.Model
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocUtils
import File.FileID
import Kontra
import KontraLink
import Mails.SendMail
import Misc
import Templates.Templates
import Templates.TemplatesUtils
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Util.MonadUtils
import DB.Classes
import Control.Monad
import Data.Functor
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
-- import Happstack.State (query)
import File.Model
import Control.Monad.Trans
import AppView
import User.Model
import Util.HasSomeCompanyInfo

mailDocumentRemind :: TemplatesMonad m
                   => Maybe BS.ByteString
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> m Mail
mailDocumentRemind cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned True cm c d s
         _                                       -> remindMailSigned    True cm c d s

mailDocumentRemindContent :: TemplatesMonad m
                          => Maybe BS.ByteString
                          -> Context
                          -> Document
                          -> SignatoryLink
                          -> m String
mailDocumentRemindContent cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
         _                                       -> remindMailSignedContent False cm c d s

remindMailNotSigned :: TemplatesMonad m
                    => Bool
                    -> Maybe BS.ByteString
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> m Mail
remindMailNotSigned forMail customMessage ctx document signlink = do
    let mainfile =  head $ (documentfiles document) ++ [FileID 0]
    authorattachmentfiles <- mapM (ioRunDB (ctxdbconn ctx) . dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailremindnotsigned) $ do
        fieldM "header" $ do
            header <- if isNothing customMessage
                         then remindMailNotSignedStandardHeader document signlink
                         else return $ BS.toString $ fromJust customMessage
            makeEditable "customtext" header
        fieldM "footer" $ mailFooterForInviter ctx document
        field "partners" $ map (BS.toString . getSmartName) $ partyList document
        field "partnerswhosigned" $ map (BS.toString . getSmartName) $ partySignedList document
        field "someonesigned" $ not $ null $ partySignedList document
        field "timetosign" $ show <$> documenttimeouttime document
        fieldM "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        field "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just signlink <| forMail |> Nothing) (mainfile)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        field "sigattachments" $ for (documentsignatoryattachments document) $ \sa ->
                        (BS.toString $ signatoryattachmentname sa, BS.toString <$> getSmartName <$> getMaybeSignatoryLink (document, signatoryattachmentemail sa))
        field "nojavascriptmagic" $ True
        field "javascriptmagic" $ False
        field "companyname" $ nothingIfEmpty $ getCompanyName document


remindMailSigned :: TemplatesMonad m
                 => Bool
                 -> Maybe BS.ByteString
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> m Mail
remindMailSigned _forMail customMessage ctx document signlink = do
    sheader <- remindMailSignedStandardHeader document signlink
    documentMailWithDocLocale ctx document "remindMailSigned" $ do
            fieldM "header" $ makeEditable "customtext" $  fromMaybe sheader (BS.toString <$> customMessage)
            fieldM "footer" $ mailFooterForInviter ctx document


remindMailNotSignedContent :: TemplatesMonad m
                           => Bool
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink =
    (BS.toString . content) <$> remindMailNotSigned forMail customMessage ctx document signlink

remindMailSignedContent :: TemplatesMonad m
                        => Bool
                        -> (Maybe BS.ByteString)
                        -> Context
                        -> Document
                        -> SignatoryLink
                        -> m String
remindMailSignedContent forMail customMessage ctx document signlink = do
    (BS.toString . content) <$> remindMailSigned forMail customMessage ctx document signlink

remindMailSignedStandardHeader :: TemplatesMonad m
                               => Document
                               -> SignatoryLink
                               -> m String
remindMailSignedStandardHeader document signlink =
    renderLocalTemplateForProcess document processmailsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe BS.empty getSmartName mauthorsiglink
        field "documenttitle" $ BS.toString $ documenttitle document
        field "author" $ BS.toString creatorname
        field "personname" $ BS.toString $ getSmartName signlink
        field "service" $ isJust $ documentservice document

remindMailNotSignedStandardHeader :: TemplatesMonad m
                                  => Document
                                  -> SignatoryLink
                                  -> m String
remindMailNotSignedStandardHeader document signlink =
    renderLocalTemplateForProcess document processmailnotsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe (BS.fromString "") getSmartName mauthorsiglink
        field "documenttitle" $ BS.toString $ documenttitle document
        field "author" $ BS.toString creatorname
        field "personname" $ BS.toString $ getSmartName signlink
        field "service" $ isJust $ documentservice document


mailDocumentRejected :: TemplatesMonad m
                     => Maybe String
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailDocumentRejected customMessage ctx document rejector = do
   documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailreject) $ do
        field "rejectorName" $ getSmartName rejector
        fieldM "footer" $ mailFooterForInvitee ctx document
        field "customMessage" $ customMessage
        field "companyname" $ nothingIfEmpty $ getCompanyName document



mailRejectMailContent :: TemplatesMonad m
                      => Maybe String
                      -> Context
                      -> Document
                      -> SignatoryLink
                      -> m String
mailRejectMailContent customMessage ctx  document rejector =
     (BS.toString . content) <$> mailDocumentRejected customMessage ctx document rejector

mailDocumentErrorForAuthor :: (HasLocale a, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentErrorForAuthor ctx document authorlocale = do
   documentMail authorlocale ctx document "mailDocumentError" $ return ()

mailDocumentErrorForSignatory :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentErrorForSignatory ctx document = do
   documentMailWithDocLocale ctx document "mailDocumentError" $ return ()


data InvitationTo = Sign | View deriving (Eq,Show)

fieldsInvitationTo :: (TemplatesMonad m) => InvitationTo -> Fields m
fieldsInvitationTo a = do
    field "sign" (a == Sign)
    field "view" (a == View)

mailInvitation :: TemplatesMonad m
                                  => Bool
                                  -> Context
                                  -> InvitationTo
                                  -> Document
                                  -> Maybe SignatoryLink
                                  -> m Mail
mailInvitation forMail
               ctx
               invitationto
               document@Document{documentinvitetext, documenttitle }
               msiglink = do
    authorattachmentfiles <- mapM (ioRunDB (ctxdbconn ctx) . dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    let creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
    let issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
    let personname = maybe "" (BS.toString . getSmartName) msiglink
    let mainfile =  head $ (documentfiles document) ++ [FileID 0] -- There always should be main file but tests fail without it
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailinvitationtosign) $ do
        fieldsInvitationTo invitationto
        field "nojavascriptmagic" $ forMail
        field "javascriptmagic" $ not forMail
        fieldM "header" $ do
            header <- if BS.null documentinvitetext
                         then if issignatory || not forMail
                                 then renderLocalTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                                     field "creatorname" $ creatorname
                                     field "personname" $ personname
                                     field "documenttitle" $ BS.toString documenttitle
                                     field "service" $ isJust $ documentservice document
                                 else renderLocalTemplateFM document "mailInvitationToViewDefaultHeader" $ do
                                     field "creatorname" creatorname
                                     field "personname" personname
                                     field "documenttitle" $ BS.toString documenttitle
                                     field "service" $ isJust $ documentservice document
                         else return $ BS.toString documentinvitetext
            makeEditable "customtext" header
        fieldM "footer" $ mailFooterForInviter ctx document
        fieldM "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "partners" $ map (BS.toString . getSmartName) $ partyList document
        field "partnerswhosigned" $ map (BS.toString . getSmartName) $ partySignedList document
        field "someonesigned" $ not $ null $ partySignedList document
        field "timetosign" $ show <$> documenttimeouttime document
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        field "previewLink" $ show $ LinkDocumentPreview (documentid document) (msiglink <| forMail |> Nothing) (mainfile)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        field "sigattachments" $ for (documentsignatoryattachments document) $ \sa ->
                        (BS.toString $ signatoryattachmentname sa, BS.toString <$> getSmartName <$> getMaybeSignatoryLink (document, signatoryattachmentemail sa))
        field "companyname" $ nothingIfEmpty $ getCompanyName document



mailInvitationContent :: TemplatesMonad m
                     => Bool
                     -> Context
                     -> InvitationTo
                     -> Document
                     -> Maybe SignatoryLink
                     -> m String
mailInvitationContent  forMail ctx invitationto document msiglink = do
     (BS.toString . content) <$> mailInvitation forMail ctx invitationto document msiglink

mailDocumentClosed :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentClosed ctx document= do
   partylist <- renderLocalListTemplate document $ map (BS.toString . getSmartName) $ partyList document
   documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailclosed) $ do
        field "partylist" $ partylist
        fieldM "footer" $ mailFooterForService ctx document
        field "companyname" $ nothingIfEmpty $ getCompanyName document


mailDocumentAwaitingForAuthor :: (HasLocale a, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentAwaitingForAuthor ctx document authorlocale = do
    signatories <- renderLocalListTemplate authorlocale $ map (BS.toString . getSmartName) $ partySignedList document
    documentMail authorlocale ctx document "mailDocumentAwaitingForAuthor" $ do
        field "authorname" $ BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
        field "documentlink" $ (ctxhostpart ctx) ++ (show $ LinkIssueDoc $ documentid document)
        field "partylist" signatories
        field "companyname" $ nothingIfEmpty $ getCompanyName document


mailCancelDocumentContent :: TemplatesMonad m
                                  => Bool
                                  -> (Maybe BS.ByteString)
                                  -> Context
                                  -> Document
                                  -> m String
mailCancelDocumentContent forMail customMessage ctx document =
    (BS.toString . content) <$> mailCancelDocument forMail customMessage ctx document

mailCancelDocument :: TemplatesMonad m
                           => Bool
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> m Mail
mailCancelDocument _forMail customMessage ctx document = do
    mail <- documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailcancel) $ do
        fieldM "header" $ do
            header <- case customMessage of
                           Just c -> return $ BS.toString c
                           Nothing -> renderLocalTemplateForProcess document processmailcancelstandardheader $ do
                               field "partylist" $ map (BS.toString . getSmartName) $ partyList document
            makeEditable "customtext" header
        fieldM "footer" $ mailFooterForInviter ctx document
        field "companyname" $ nothingIfEmpty $ getCompanyName document
    return $ mail { from = documentservice document}

mailMismatchSignatory :: TemplatesMonad m
                        => Context
                        -> Document
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> Bool
                        -> m Mail
mailMismatchSignatory ctx document authoremail authorname doclink signame badname msg isbad = do
   documentMailWithDocLocale ctx document "mailMismatchSignatory" $ do
        field "authorname" authorname
        field "signame" signame
        field "badname" badname
        field "authoremail" authoremail
        field "doclink" doclink
        field "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing)

mailMismatchAuthor :: (HasLocale a, TemplatesMonad m) => Context -> Document -> String -> String -> String -> a -> m Mail
mailMismatchAuthor ctx document authorname badname bademail authorlocale = do
    let Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    documentMail authorlocale ctx document "mailMismatchAuthor" $ do
        field "messages" $ concat $ map para $ lines msg
        field "authorname" authorname
        field "doclink" $ ctxhostpart ctx ++ (show $ LinkDesignDoc $ DesignStep2 (documentid document)
                                                                    Nothing
                                                                    Nothing
                                                                    (not (hasSigned (getAuthorSigLink document))))
        field "bademail" bademail
        field "badname" badname

-- helpers

makeEditable :: TemplatesMonad m => String -> String -> m String
makeEditable name this =
    renderTemplateFM "makeEditable" $ do
        field "name" name
        field "this" this

{- |
    Use for anyone sending a mail as a person from the inviting side.  This could be the author
    in the case of the invitation mails, and it could be a company admin withdrawing a document
    that was authored within their company by another person.
    The footer is in order of preference
      1. a custom footer configured on context user (there should be one!)
      2. the custom footer saved on the document itself (setup for Upsales originally)
      3. a custom footer configured for the document's service
      4. the default powered by scrive footer
-}
mailFooterForInviter :: TemplatesMonad m => Context -> Document -> m String
mailFooterForInviter ctx doc = do
  servicefooter <- getServiceFooter ctx doc
  firstOrDefaultFooter ctx doc [ ctxmaybeuser ctx >>= getUserFooter
                               , getDocumentFooter doc
                               , servicefooter
                               ]

{- |
    Use for anyone sending a mail as a person invited to that document.  For example when rejecting
    a document.
    The footer is in order of preference:
      1. a custom footer configured on the context user (if there is one)
      3. the default powered by scrive footer
-}
mailFooterForInvitee :: TemplatesMonad m => Context -> Document -> m String
mailFooterForInvitee ctx doc =
  firstOrDefaultFooter ctx doc [ ctxmaybeuser ctx >>= getUserFooter
                               ]

{- |
    Use when the mail about the document is from the service rather than from a specific person.
    For example when sending out document close confirmations.
    The footer is in order of preferece:
      1. the custom footer saved on the document itself (setup for Upsales originally)
      2. a custom footer configured for the document's service
      3. the default powered by scrive footer
-}
mailFooterForService :: TemplatesMonad m => Context -> Document -> m String
mailFooterForService ctx doc = do
  servicefooter <- getServiceFooter ctx doc
  firstOrDefaultFooter ctx doc [ getDocumentFooter doc
                               , servicefooter
                               ]

firstOrDefaultFooter :: TemplatesMonad m => Context -> Document -> [Maybe String] -> m String
firstOrDefaultFooter ctx doc mfooters = do
  poweredbyscrivefooter <- getPoweredByScriveFooter ctx doc
  return . fromMaybe poweredbyscrivefooter $ msum mfooters

getUserFooter :: User -> Maybe String
getUserFooter = customfooter . usersettings

getDocumentFooter :: Document -> Maybe String
getDocumentFooter = fmap BS.toString . documentmailfooter . documentui

getServiceFooter :: MonadIO m => Context -> Document -> m (Maybe String)
getServiceFooter ctx doc = do
  mservice <- liftMM (ioRunDB (ctxdbconn ctx) . dbQuery . GetService) (return $ documentservice doc)
  return . fmap BS.toString $ mservice >>= servicemailfooter . serviceui

getPoweredByScriveFooter :: TemplatesMonad m => Context -> Document -> m String
getPoweredByScriveFooter ctx doc = renderLocalTemplateFM doc "poweredBySkrivaPaPara" $ field "ctxhostpart" $ ctxhostpart ctx

makeFullLink :: TemplatesMonad m => Context -> Document -> String -> m String
makeFullLink ctx doc link = do
    mservice <- liftMM (ioRunDB (ctxdbconn ctx) . dbQuery . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ BS.toString location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link


documentMailWithDocLocale :: (MonadIO m,Functor m,TemplatesMonad m) => Context -> Document -> String -> Fields m -> m Mail
documentMailWithDocLocale ctx doc mailname otherfields = documentMail doc ctx doc mailname otherfields

documentMail :: (MonadIO m,Functor m,TemplatesMonad m, HasLocale a) =>  a -> Context -> Document -> String -> Fields m -> m Mail
documentMail haslocale ctx doc mailname otherfields = do
    mservice <- liftMM (ioRunDB (ctxdbconn ctx) . dbQuery . GetService) (return $ documentservice doc)
    let allfields = do
        contextFields ctx
        field "documenttitle" $ BS.toString $ documenttitle doc
        field "creatorname" $ BS.toString $ getSmartName $ fromJust $ getAuthorSigLink doc
        when (isJust mservice) $
            fieldF "service" $ serviceFields "" mservice
        otherfields
    kontramaillocal haslocale mailname allfields
