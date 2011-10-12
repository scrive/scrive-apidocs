module Doc.DocViewMail (
      mailCancelDocumentByAuthor
    , mailCancelDocumentByAuthorContent
    , mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentError
    , mailDocumentRejected
    , mailDocumentRemind
    , mailDocumentRemindContent
    , InvitationTo(..)
    , mailInvitation
    , mailInvitationContent
    , mailMismatchAuthor
    , mailMismatchSignatory
    , mailRejectMailContent
    , mailMailAPIConfirm
    , mailMailApiError
    ) where

import API.Service.Model
import Doc.DocProcess
import Doc.DocState
import Doc.DocUtils
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
import Happstack.State (query)
import File.State
import Control.Monad.Trans

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
    let creatorname = maybe "" (BS.toString . getSmartName) $ getAuthorSigLink document
    authorattachmentfiles <- mapM (query . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    kontramail (fromMaybe "" $ getValueForProcess document processmailremindnotsigned) $ do
        fieldM "header" $ do
            header <- if isNothing customMessage
                         then remindMailNotSignedStandardHeader document signlink
                         else return $ BS.toString $ fromJust customMessage
            makeEditable "customtext" header
        fieldM "footer" $ do
            if forMail
               then if (isNothing customMessage)
                       then mailFooter ctx document
                       else renderTemplateFM "customFooter" $
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplateFM "customFooter" $
                            field "creatorname" creatorname
                        replaceOnEdit this with
        fieldM "timetosigninfo" $ do
            case documenttimeouttime document of
                 Just time -> renderTemplateFM "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing   -> return ""
        fieldM "partnersinfo" $ do
            renderListTemplate $ map (BS.toString . getSmartName) $ partyList document
        fieldM "whohadsignedinfo" $ do
            renderTemplateForProcess document processwhohadsignedinfoformail $ do
                fieldM "signedlist" $
                    if not $ null $ partySignedList document
                       then fmap Just $ renderListTemplate $ map (BS.toString . getSmartName) $ partySignedList document
                       else return Nothing
        fieldM "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        contextFields ctx

remindMailSigned :: TemplatesMonad m
                 => Bool
                 -> Maybe BS.ByteString
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> m Mail
remindMailSigned _forMail customMessage ctx document signlink = do
    sheader <- remindMailSignedStandardHeader document signlink
    kontramail "remindMailSigned" $ do
            fieldM "header" $ makeEditable "customtext" $  fromMaybe sheader (BS.toString <$> customMessage)
            fieldM "footer" $ mailFooter ctx document
            contextFields ctx
    

remindMailNotSignedContent :: TemplatesMonad m
                           => Bool
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink =
    (BS.toString . content) <$> remindMailNotSigned  forMail customMessage ctx document signlink
    
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
    renderTemplateForProcess document processmailsignedstandardheader $ do
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
    renderTemplateForProcess document processmailnotsignedstandardheader $ do
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
    kontramail (fromMaybe "" $ getValueForProcess document processmailreject) $ do
        field "rejectorName" $ getSmartName rejector
        fieldM "footer" $ mailFooter ctx document
        field "customMessage" $ customMessage
        documentMailFields True ctx document Nothing

mailRejectMailContent :: TemplatesMonad m
                      => Maybe String
                      -> Context
                      -> Document
                      -> SignatoryLink
                      -> m String
mailRejectMailContent customMessage ctx  document  rejector =
     (BS.toString . content) <$> mailDocumentRejected customMessage ctx document rejector
     
mailDocumentError :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentError ctx document = do
    kontramail "mailDocumentError" $ do
        documentMailFields True ctx document Nothing


data InvitationTo = Sign | Send | View deriving (Eq,Show)

fieldsInvitationTo :: (TemplatesMonad m) => InvitationTo -> Fields m
fieldsInvitationTo a = do
    field "sign" (a == Sign)
    field "send" (a == Send)
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
               document@Document{ documenttimeouttime, documentinvitetext, documenttitle }
               msiglink = do
    authorattachmentfiles <- mapM (query . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
    let creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
    let issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
    let personname = maybe "" (BS.toString . getSmartName) msiglink
    kontramail (fromMaybe "" $ getValueForProcess document processmailinvitationtosign) $ do
        fieldsInvitationTo invitationto
        fieldM "header" $ do
            header <- if BS.null documentinvitetext
                         then if issignatory || not forMail
                                 then renderTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                                     field "creatorname" $ creatorname
                                     field "personname" $ personname
                                     field "documenttitle" $ BS.toString documenttitle
                                     field "service" $ isJust $ documentservice document
                                 else renderTemplateFM "mailInvitationToViewDefaultHeader" $ do
                                     field "creatorname" creatorname
                                     field "personname" personname
                                     field "documenttitle" $ BS.toString documenttitle
                                     field "service" $ isJust $ documentservice document
                         else return $ BS.toString documentinvitetext
            makeEditable "customtext" header
        fieldM "footer" $ do
            if forMail
               then if BS.null documentinvitetext
                       then mailFooter ctx document
                       else renderTemplateFM "customFooter" $ do
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplateFM "customFooter" $ do
                            field "creatorname" creatorname
                        replaceOnEdit this with
        fieldM "timetosigninfo" $ do
            case documenttimeouttime of
                 Just time -> renderTemplateFM "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing -> return ""
        fieldM "partnersinfo" $ do
            if forMail
               then renderListTemplate $ map (BS.toString . getSmartName) $ partyList document
               else renderTemplateFM "updateinglistwithauthor" $ field "creatorname" creatorname
        fieldM "whohadsignedinfo" $ do
            if forMail
               then do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderListTemplate $  map (BS.toString . getSmartName) $ partySignedList document
                                    else return Nothing
                   renderTemplateForProcess document processwhohadsignedinfoformail $ do
                       field "signedlist" signedlist
               else renderTemplateM "whohadsignedinfoforpreview" ()
        fieldM "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "issignatory" $ issignatory || not forMail
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        fieldFL "sigattachments" $ do
            for (buildattach csvstring document (documentsignatoryattachments document) [])
                (\(n, _, sigs) -> do
                    field "name" n
                    fieldM "sigs" $ renderListTemplate (map (BS.toString . fst) sigs))
        documentMailFields True ctx document msiglink
            
            
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
mailDocumentClosed ctx document@Document{documenttitle} = do
   partylist <- renderListTemplate $ map (BS.toString . getSmartName) $ partyList document
   kontramail (fromMaybe "" $ getValueForProcess document processmailclosed) $ do
        field "documenttitle" $ BS.toString documenttitle
        field "partylist" $ partylist
        fieldM "footer" $ mailFooter ctx document
        field "service" $ isJust $ documentservice document
        contextFields ctx

mailDocumentAwaitingForAuthor :: TemplatesMonad m => Context -> Document  -> m Mail
mailDocumentAwaitingForAuthor ctx document@Document{documenttitle,documentid} = do
    signatories <- renderListTemplate $ map (BS.toString . getSmartName) $ partySignedList document
    kontramail "mailDocumentAwaitingForAuthor" $ do
        field "authorname" $ BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
        field "documenttitle" $ BS.toString  documenttitle
        field "documentlink" $ (ctxhostpart ctx) ++ (show $ LinkIssueDoc documentid)
        field "partylist" signatories
        contextFields ctx

mailCancelDocumentByAuthorContent :: TemplatesMonad m
                                  => Bool
                                  -> (Maybe BS.ByteString)
                                  -> Context
                                  -> Document
                                  -> m String
mailCancelDocumentByAuthorContent forMail customMessage ctx document = 
    (BS.toString . content) <$> mailCancelDocumentByAuthor forMail customMessage ctx document

mailCancelDocumentByAuthor :: TemplatesMonad m
                           => Bool 
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> m Mail
mailCancelDocumentByAuthor forMail customMessage ctx document = do
    mail <- kontramail (fromMaybe "" $ getValueForProcess document processmailcancelbyauthor) $ do
        let creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
        fieldM "header" $ do
            header <- case customMessage of
                           Just c -> return $ BS.toString c
                           Nothing -> renderTemplateForProcess document processmailcancelbyauthorstandardheader $ do
                               field "partylist" $ map (BS.toString . getSmartName) $ partyList document
            makeEditable "customtext" header
        fieldM "footer" $ do
            if forMail
               then if isNothing customMessage
                       then mailFooter ctx document
                       else renderTemplateFM "customFooter" $
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplateFM "customFooter" $
                            field "creatorname" creatorname
                        replaceOnEdit this with
        documentMailFields True ctx document Nothing
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
   kontramail "mailMismatchSignatory" $ do
        field "authorname" authorname
        field "signame" signame
        field "badname" badname
        field "authoremail" authoremail
        field "doclink" doclink
        field "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing)
        documentMailFields True ctx document Nothing

mailMismatchAuthor :: TemplatesMonad m => Context -> Document -> String -> String -> String -> m Mail
mailMismatchAuthor ctx document authorname badname bademail = do
    let Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    kontramail "mailMismatchAuthor" $ do
        field "messages" $ concat $ map para $ lines msg
        field "authorname" authorname
        field "doclink" $ ctxhostpart ctx ++ (show $ LinkDesignDoc $ DesignStep2 (documentid document)
                                                                    Nothing
                                                                    Nothing
                                                                    (not (hasSigned (getAuthorSigLink document))))
        field "bademail" bademail
        field "badname" badname
        documentMailFields True ctx document Nothing

-- helpers

makeEditable :: TemplatesMonad m => String -> String -> m String
makeEditable name this =
    renderTemplateFM "makeEditable" $ do
        field "name" name
        field "this" this

replaceOnEdit :: TemplatesMonad m => String -> String -> m String
replaceOnEdit this with =
    renderTemplateFM "replaceOnEdit" $ do
        field "this" this
        field "with" with

mailFooter :: TemplatesMonad m => Context -> Document -> m String
mailFooter ctx doc = do
    mservice <- liftMM (ioRunDB (ctxdbconn ctx) . dbQuery . GetService) (return $ documentservice doc)
    case (documentmailfooter $ documentui doc) `mplus` (join $ servicemailfooter <$> serviceui <$> mservice) of
         Just footer -> return $ BS.toString footer
         Nothing -> renderTemplateFM "poweredBySkrivaPaPara" $ field "ctxhostpart" $ ctxhostpart ctx

makeFullLink :: TemplatesMonad m => Context -> Document -> String -> m String
makeFullLink ctx doc link = do
    mservice <- liftMM (ioRunDB (ctxdbconn ctx) . dbQuery . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ BS.toString location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link

mailMailAPIConfirm :: TemplatesMonad m
                      => Context 
                      -> Document
                      -> SignatoryLink
                      -> m Mail
mailMailAPIConfirm ctx document siglink = do
  let issignatory = (elem SignatoryPartner . signatoryroles) siglink
  kontramail (fromMaybe "" $ getValueForProcess document processmailconfirmbymailapi)  $ do
        fieldM "footer" $ mailFooter ctx document
        fieldM "timetosigninfo" $ do
            case (documenttimeouttime document) of
                 Just time -> renderTemplateFM "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing -> return ""
        fieldM "partnersinfo" $ do
             renderListTemplate $ map (BS.toString . getSmartName) $ partyList document
        fieldM "whohadsignedinfo" $ do
             do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderListTemplate $  map (BS.toString . getSmartName) $ partySignedList document
                                    else return Nothing
                   renderTemplateForProcess document processwhohadsignedinfoformail $ do
                       field "signedlist" signedlist
        field "issignatory" $ issignatory 
        field "isattachments" $ False
        field "hassigattachments" $ False
        field "link" $ ctxhostpart ctx ++ (show $  LinkIssueDoc (documentid document))
        documentMailFields True ctx document (Just siglink)
                   

mailMailApiError:: MailAddress -> String -> Mail
mailMailApiError from err = 
      emptyMail  {  to = [from]
                  , title   = BS.fromString "Error while parsing request"
                  , content = BS.fromString err
                 }
                 
documentMailFields :: (MonadIO m) => Bool -> Context -> Document -> (Maybe SignatoryLink) -> Fields m
documentMailFields _forMail ctx document _mslink = do
    contextFields ctx
    field "documenttitle" $ BS.toString $ documenttitle document
    field "creatorname" $ BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
    
