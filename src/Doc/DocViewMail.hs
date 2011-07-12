module Doc.DocViewMail (
      mailCancelDocumentByAuthor
    , mailCancelDocumentByAuthorContent
    , mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentError
    , mailDocumentRejected
    , mailDocumentRemind
    , mailDocumentRemindContent
    , mailInvitationToSend
    , mailInvitationToSign
    , mailInvitationToSignOrViewContent
    , mailInvitationToView
    , mailMismatchAuthor
    , mailMismatchSignatory
    , mailRejectMailContent
    ) where

import API.Service.ServiceState
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

import Control.Monad
import Data.Functor
import Data.Maybe
import Happstack.State (query)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

mailDocumentRemind :: TemplatesMonad m
                   => Maybe BS.ByteString
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> m Mail
mailDocumentRemind cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned cm c d s
         _                                       -> remindMailSigned    cm c d s

mailDocumentRemindContent :: TemplatesMonad m
                          => Maybe BS.ByteString
                          -> Context
                          -> Document
                          -> SignatoryLink
                          -> m String
mailDocumentRemindContent cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
         _                                       -> remindMailSignedContent cm c d s

remindMailNotSigned :: TemplatesMonad m
                    => Maybe BS.ByteString
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> m Mail
remindMailNotSigned customMessage ctx document@Document{documenttitle} signlink = do
    title <- renderTemplateFM "remindMailNotSignedTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle
    content <- wrapHTML' =<< remindMailNotSignedContent True customMessage ctx document signlink
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

remindMailSigned :: TemplatesMonad m
                 => Maybe BS.ByteString
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> m Mail
remindMailSigned customMessage ctx document@Document{documenttitle}  signlink = do
    title <- renderTemplateFM "remindMailSignedTitle" $ do
        field "documenttitle" documenttitle
    content <- wrapHTML' =<< remindMailSignedContent customMessage ctx  document signlink
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

remindMailNotSignedContent :: TemplatesMonad m
                           => Bool
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink = do
    renderTemplateForProcess document processmailremindnotsignedcontent $ do
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
        field "creatorname" creatorname
        field "documenttitle" $ BS.toString $ documenttitle document
        fieldM "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename . authorattachmentfile) (documentauthorattachments document)
    where
        creatorname = maybe "" (BS.toString . getSmartName) $ getAuthorSigLink document

remindMailSignedContent :: TemplatesMonad m
                        => (Maybe BS.ByteString)
                        -> Context
                        -> Document
                        -> SignatoryLink
                        -> m String
remindMailSignedContent customMessage ctx document signlink = do
    header <- if (isNothing customMessage)
                 then remindMailSignedStandardHeader document signlink
                 else return . BS.toString $ fromJust customMessage
    editableheader <- makeEditable "customtext" header
    footer <- mailFooter ctx document
    return $ editableheader ++ footer

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

mailDocumentRejected :: TemplatesMonad m
                     => Maybe String
                     -> Context
                     -> BS.ByteString
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailDocumentRejected customMessage ctx username document@Document{documenttitle}  rejector = do
    title <- renderTemplateFM "mailRejectMailTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML' =<< mailRejectMailContent customMessage ctx username document rejector
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent :: TemplatesMonad m
                      => Maybe String
                      -> Context
                      -> BS.ByteString
                      -> Document
                      -> SignatoryLink
                      -> m String
mailRejectMailContent customMessage ctx  username  document  rejector =
    renderTemplateForProcess document processmailrejectcontent $ do
        field "username" username
        field "documenttitle" $ documenttitle document
        field "rejectorName" $ getSmartName rejector
        fieldM "footer" $ mailFooter ctx document
        field "customMessage" $ customMessage

mailDocumentError :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentError ctx document@Document{documenttitle} = do
    title <- renderTemplateFM "mailDocumentErrorTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML' =<< mailDocumentErrorContent ctx document
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentErrorContent :: TemplatesMonad m => Context -> Document -> m String
mailDocumentErrorContent ctx document =
    -- FIXME: should have author name here also
    renderTemplateFM "mailDocumentErrorContent" $ do
        -- field "authorname" $ BS.toString $ authorname
        field "documenttitle" $ BS.toString $ documenttitle document
        field "ctxhostpart" $ ctxhostpart ctx

mailInvitationToSignOrViewContent :: TemplatesMonad m
                                  => Bool
                                  -> Context
                                  -> Document
                                  -> Maybe SignatoryLink
                                  -> m String
mailInvitationToSignOrViewContent forMail
                                  ctx
                                  document@Document{ documenttimeouttime, documentinvitetext, documenttitle }
                                  msiglink = do
    csvstring <- renderTemplateM "csvsendoutsignatoryattachmentstring" ()
    renderTemplateForProcess document processmailinvitationtosigncontent $ do
        fieldM "header" $ do
            header <- if BS.null documentinvitetext
                         then if issignatory || not forMail
                                 then renderTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                                     field "creatorname" $ creatorname
                                     field"personname" $ personname
                                     field "documenttitle" $ BS.toString documenttitle
                                 else renderTemplateFM "mailInvitationToViewDefaultHeader" $ do
                                     field "creatorname" creatorname
                                     field "personname" personname
                                     field "documenttitle" $ BS.toString documenttitle
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
        field "documenttitle" $ BS.toString documenttitle
        fieldM "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "issignatory" $ issignatory || not forMail
        field "creatorname" creatorname
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename . authorattachmentfile) (documentauthorattachments document)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        fieldFL "sigattachments" $ do
            for (buildattach csvstring document (documentsignatoryattachments document) [])
                (\(n, _, sigs) -> do
                    field "name" n
                    fieldM "sigs" $ renderListTemplate (map (BS.toString . fst) sigs))
        where
            creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
            issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
            personname = maybe "" (BS.toString . getSmartName) msiglink

mailInvitationToSign :: TemplatesMonad m
                     => Context
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailInvitationToSign ctx document@Document{documenttitle} siglink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplateFM "mailInvitationToSignTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $ BS.toString authorname
    content <- wrapHTML' =<< mailInvitationToSignOrViewContent True ctx document (Just siglink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToSend :: TemplatesMonad m
                     => Context
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailInvitationToSend ctx document@Document{documenttitle} signaturelink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplateFM "mailInvitationToSignTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $ BS.toString authorname
    content <- wrapHTML' =<< mailInvitationToSignOrViewContent True ctx document (Just signaturelink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToView :: TemplatesMonad m
                     => Context
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailInvitationToView ctx document@Document{documenttitle} signaturelink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplateFM "mailInvitationToViewTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $ BS.toString authorname
    content <- wrapHTML' =<< mailInvitationToSignOrViewContent True ctx document (Just signaturelink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentClosed :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentClosed ctx document@Document{documenttitle} = do
    title <- renderTemplateFM "mailDocumentClosedTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    partylist <- renderListTemplate $ map (BS.toString . getSmartName) $ partyList document
    content <- wrapHTML' =<< (renderTemplateForProcess document processmailclosedcontent $ do
        field "documenttitle" $ BS.toString documenttitle
        field "partylist" $ partylist
        fieldM "footer" $ mailFooter ctx document)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentAwaitingForAuthor :: TemplatesMonad m => Context -> BS.ByteString -> Document  -> m Mail
mailDocumentAwaitingForAuthor (Context {ctxhostpart}) authorname  document@Document{documenttitle,documentid} = do
    signatories <- renderListTemplate $ map (BS.toString . getSmartName) $ partySignedList document
    title <- renderTemplateFM "mailDocumentAwaitingForAuthorTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML' =<< (renderTemplateFM "mailDocumentAwaitingForAuthorContent" $ do
        field "authorname" $ BS.toString authorname
        field "documenttitle" $ BS.toString  documenttitle
        field "ctxhostpart" ctxhostpart
        field "documentlink" $ ctxhostpart ++ (show $ LinkIssueDoc documentid)
        field "partylist" signatories)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailCancelDocumentByAuthorContent :: TemplatesMonad m
                                  => Bool
                                  -> (Maybe BS.ByteString)
                                  -> Context
                                  -> Document
                                  -> m String
mailCancelDocumentByAuthorContent forMail customMessage ctx document = do
    renderTemplateForProcess document processmailcancelbyauthorcontent $ do
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
        field "creatorname" creatorname
        field "documenttitle" $ BS.toString $ documenttitle document
    where
        creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document

mailCancelDocumentByAuthor :: TemplatesMonad m
                           => Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m Mail
mailCancelDocumentByAuthor customMessage ctx document@Document{documenttitle} signlink = do
    title <- renderTemplateFM "mailCancelDocumentByAuthorTitle" $ do
        field "documenttitle" documenttitle
    content <- wrapHTML' =<< mailCancelDocumentByAuthorContent True customMessage ctx document
    return $ emptyMail { title = BS.fromString title, to = [getMailAddress signlink], content = BS.fromString content }

mailMismatchSignatory :: TemplatesMonad m
                        => Document
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> Bool
                        -> m Mail
mailMismatchSignatory document authoremail authorname doclink signame badname msg isbad = do
    title <- renderTemplateFM "mailMismatchSignatoryTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML' =<< (renderTemplateFM "mailMismatchSignatoryContent" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
        field "authorname" authorname
        field "signame" signame
        field "badname" badname
        field "authoremail" authoremail
        field "doclink" doclink
        field "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing))

    return $ emptyMail  { title = BS.fromString title
                        , content = BS.fromString content
                        }

mailMismatchAuthor :: TemplatesMonad m => Context -> Document -> String -> String -> String -> m Mail
mailMismatchAuthor ctx document authorname badname bademail = do
    let Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    title <- renderTemplateFM "mailMismatchAuthorTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML' =<< (renderTemplateFM "mailMismatchAuthorContent" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
        field "messages" $ concat $ map para $ lines msg
        field "authorname" authorname
        field "doclink" $ ctxhostpart ctx ++ (show $ LinkDesignDoc $ DesignStep2 (documentid document)
                                                                    Nothing
                                                                    Nothing
                                                                    (not (hasSigned (getAuthorSigLink document))))
        field "bademail" bademail
        field "badname" badname)
    return $ emptyMail { title = BS.fromString title, content = BS.fromString content }

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
    mservice <- liftMM (query . GetService) (return $ documentservice doc)
    case (documentmailfooter $ documentui doc) `mplus` (join $ servicemailfooter <$> serviceui <$> mservice) of
         Just footer -> return $ BS.toString footer
         Nothing -> renderTemplateFM "poweredBySkrivaPaPara" $ field "ctxhostpart" $ ctxhostpart ctx

makeFullLink :: TemplatesMonad m => Context -> Document -> String -> m String
makeFullLink ctx doc link = do
    mservice <- liftMM (query . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ BS.toString location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link
