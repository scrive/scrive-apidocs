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

import Control.Monad
import Data.Functor
import Data.Maybe
import Happstack.State (query)
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

mailDocumentRemind :: KontrakcjaTemplates
                   -> Maybe BS.ByteString
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> IO Mail
mailDocumentRemind templates cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned templates cm c d s
         _                                       -> remindMailSigned    templates cm c d s

mailDocumentRemindContent :: KontrakcjaTemplates
                          -> Maybe BS.ByteString
                          -> Context
                          -> Document
                          -> SignatoryLink
                          -> IO String
mailDocumentRemindContent templates cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent templates False cm c d s
         _                                       -> remindMailSignedContent templates cm c d s

remindMailNotSigned :: KontrakcjaTemplates
                    -> Maybe BS.ByteString
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> IO Mail
remindMailNotSigned templates customMessage ctx document@Document{documenttitle} signlink = do
    title <- renderTemplate templates "remindMailNotSignedTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle
    content <- wrapHTML templates =<< remindMailNotSignedContent templates True customMessage ctx document signlink
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

remindMailSigned :: KontrakcjaTemplates
                 -> Maybe BS.ByteString
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> IO Mail
remindMailSigned templates customMessage ctx document@Document{documenttitle}  signlink = do
    title <- renderTemplate templates "remindMailSignedTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle
    content <- wrapHTML templates =<< remindMailSignedContent templates customMessage ctx  document signlink
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

remindMailNotSignedContent :: KontrakcjaTemplates
                           -> Bool
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> IO String
remindMailNotSignedContent templates forMail customMessage ctx document signlink = do
    renderTemplateForProcess templates document processmailremindnotsignedcontent $ do
        field "header" $ do
            header <- if isNothing customMessage
                         then remindMailNotSignedStandardHeader templates document signlink
                         else return $ BS.toString $ fromJust customMessage
            makeEditable templates "customtext" header
        field "footer" $ do
            if forMail
               then if (isNothing customMessage)
                       then mailFooter ctx document
                       else renderTemplate templates "customFooter" $
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplate templates "customFooter" $
                            field "creatorname" creatorname
                        replaceOnEdit templates this with
        field "timetosigninfo" $ do
            case documenttimeouttime document of
                 Just time -> renderTemplate templates "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing   -> return ""
        field "partnersinfo" $ do
            renderListTemplate templates $ map (BS.toString . getSmartName) $ partyList document
        field "whohadsignedinfo" $ do
            renderTemplateForProcess templates document processwhohadsignedinfoformail $ do
                field "signedlist" $
                    if not $ null $ partySignedList document
                       then fmap Just $ renderListTemplate templates $ map (BS.toString . getSmartName) $ partySignedList document
                       else return Nothing
        field "creatorname" creatorname
        field "documenttitle" $ BS.toString $ documenttitle document
        field "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename . authorattachmentfile) (documentauthorattachments document)
    where
        creatorname = maybe "" (BS.toString . getSmartName) $ getAuthorSigLink document

remindMailSignedContent :: KontrakcjaTemplates
                        -> (Maybe BS.ByteString)
                        -> Context
                        -> Document
                        -> SignatoryLink
                        -> IO String
remindMailSignedContent templates customMessage ctx document signlink = do
    header <- if (isNothing customMessage)
                 then remindMailSignedStandardHeader templates document signlink
                 else return . BS.toString $ fromJust customMessage
    editableheader <- makeEditable templates "customtext" header
    footer <- mailFooter  ctx document
    return $ editableheader ++ footer

remindMailSignedStandardHeader :: KontrakcjaTemplates
                               -> Document
                               -> SignatoryLink
                               -> IO String
remindMailSignedStandardHeader templates document signlink =
    renderTemplateForProcess templates document processmailsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe BS.empty getSmartName mauthorsiglink
        field "documenttitle" $ BS.toString $ documenttitle document
        field "author" $ BS.toString creatorname
        field "personname" $ BS.toString $ getSmartName signlink

remindMailNotSignedStandardHeader :: KontrakcjaTemplates
                                  -> Document
                                  -> SignatoryLink
                                  -> IO String
remindMailNotSignedStandardHeader templates document signlink =
    renderTemplateForProcess templates document processmailnotsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe (BS.fromString "") getSmartName mauthorsiglink
        field "documenttitle" $ BS.toString $ documenttitle document
        field "author" $ BS.toString creatorname
        field "personname" $ BS.toString $ getSmartName signlink

mailDocumentRejected :: KontrakcjaTemplates
                     -> Maybe String
                     -> Context
                     -> BS.ByteString
                     -> Document
                     -> SignatoryLink
                     -> IO Mail
mailDocumentRejected templates customMessage ctx username document@Document{documenttitle}  rejector = do
    title <- renderTemplate templates "mailRejectMailTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML templates =<< mailRejectMailContent templates customMessage ctx username document rejector
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailRejectMailContent :: KontrakcjaTemplates
                      -> Maybe String
                      -> Context
                      -> BS.ByteString
                      -> Document
                      -> SignatoryLink
                      -> IO String
mailRejectMailContent templates customMessage ctx  username  document  rejector =
    renderTemplateForProcess templates document processmailrejectcontent $ do
        field "username" username
        field "documenttitle" $ documenttitle document
        field "rejectorName" $ getSmartName rejector
        field "footer" $ mailFooter ctx document
        field "customMessage" $ customMessage

mailDocumentError :: KontrakcjaTemplates -> Context -> Document -> IO Mail
mailDocumentError templates ctx document@Document{documenttitle} = do
    title <- renderTemplate templates "mailDocumentErrorTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML templates =<< mailDocumentErrorContent templates ctx document
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentErrorContent :: KontrakcjaTemplates -> Context -> Document -> IO String
mailDocumentErrorContent templates ctx document =
    -- FIXME: should have author name here also
    renderTemplate templates "mailDocumentErrorContent" $ do
        -- field "authorname" $ BS.toString $ authorname
        field "documenttitle" $ BS.toString $ documenttitle document
        field "ctxhostpart" $ ctxhostpart ctx

mailInvitationToSignOrViewContent :: KontrakcjaTemplates
                                  -> Bool
                                  -> Context
                                  -> Document
                                  -> Maybe SignatoryLink
                                  -> IO String
mailInvitationToSignOrViewContent templates
                                  forMail
                                  ctx
                                  document@Document{ documenttimeouttime, documentinvitetext, documenttitle }
                                  msiglink = do
    csvstring <- renderTemplate templates "csvsendoutsignatoryattachmentstring" ()
    renderTemplateForProcess templates document processmailinvitationtosigncontent $ do
        field "header" $ do
            header <- if BS.null documentinvitetext
                         then if issignatory || not forMail
                                 then renderTemplateForProcess templates document processmailinvitationtosigndefaultheader $ do
                                     field "creatorname" $ creatorname
                                     field"personname" $ personname
                                     field "documenttitle" $ BS.toString documenttitle
                                 else renderTemplate templates "mailInvitationToViewDefaultHeader" $ do
                                     field "creatorname" creatorname
                                     field "personname" personname
                                     field "documenttitle" $ BS.toString documenttitle
                         else return $ BS.toString documentinvitetext
            makeEditable templates "customtext" header
        field "footer" $ do
            if forMail
               then if BS.null documentinvitetext
                       then mailFooter ctx document
                       else renderTemplate templates "customFooter" $ do
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplate templates "customFooter" $ do
                            field "creatorname" creatorname
                        replaceOnEdit templates this with
        field "timetosigninfo" $ do
            case documenttimeouttime of
                 Just time -> renderTemplate templates "timetosigninfo" $ do
                                  field "time" $ show time
                 Nothing -> return ""
        field "partnersinfo" $ do
            if forMail
               then renderListTemplate templates $ map (BS.toString . getSmartName) $ partyList document
               else renderTemplate templates "updateinglistwithauthor" $ field "creatorname" creatorname
        field "whohadsignedinfo" $ do
            if forMail
               then do
                   signedlist <- if (not $ null $ partySignedList document)
                                    then fmap Just $ renderListTemplate templates $  map (BS.toString . getSmartName) $ partySignedList document
                                    else return Nothing
                   renderTemplateForProcess templates document processwhohadsignedinfoformail $ do
                       field "signedlist" signedlist
               else renderTemplate templates "whohadsignedinfoforpreview" ()
        field "documenttitle" $ BS.toString documenttitle
        field "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "issignatory" $ issignatory || not forMail
        field "creatorname" creatorname
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename . authorattachmentfile) (documentauthorattachments document)
        field "hassigattachments" $ length (documentsignatoryattachments document ) > 0
        field "sigattachments" $ do
            for (buildattach csvstring document (documentsignatoryattachments document) [])
                (\(n, _, sigs) -> do
                    field "name" n
                    field "sigs" $ renderListTemplate templates (map (BS.toString . fst) sigs))
        where
            creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document
            issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
            personname = maybe "" (BS.toString . getSmartName) msiglink

mailInvitationToSign :: KontrakcjaTemplates
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> IO Mail
mailInvitationToSign templates ctx document@Document { documenttitle } siglink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplate templates "mailInvitationToSignTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $ BS.toString authorname
    content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just siglink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToSend :: KontrakcjaTemplates
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> IO Mail
mailInvitationToSend templates ctx document@Document{documenttitle} signaturelink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplate templates "mailInvitationToSignTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $BS.toString authorname
    content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just signaturelink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailInvitationToView :: KontrakcjaTemplates
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> IO Mail
mailInvitationToView templates ctx document@Document{documenttitle} signaturelink = do
    let Just authorsiglink = getAuthorSigLink document
        authorname = getSmartName authorsiglink
    title <- renderTemplate templates "mailInvitationToViewTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
        field "creatorname" $ BS.toString authorname
    content <- wrapHTML templates =<< mailInvitationToSignOrViewContent templates True ctx document (Just signaturelink)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentClosed :: KontrakcjaTemplates -> Context -> Document -> IO Mail
mailDocumentClosed templates ctx document@Document{documenttitle} = do
    title <- renderTemplate templates "mailDocumentClosedTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    partylist <- renderListTemplate templates $  map (BS.toString . getSmartName) $ partyList document
    content <- wrapHTML templates =<< (renderTemplateForProcess templates document processmailclosedcontent $ do
        field "documenttitle" $ BS.toString documenttitle
        field "partylist" $ partylist
        field "footer" $ mailFooter ctx document)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailDocumentAwaitingForAuthor :: KontrakcjaTemplates -> Context -> BS.ByteString -> Document  -> IO Mail
mailDocumentAwaitingForAuthor templates (Context {ctxhostpart}) authorname  document@Document{documenttitle,documentid} = do
    signatories <- renderListTemplate templates $ map (BS.toString . getSmartName) $ partySignedList document
    title <- renderTemplate templates "mailDocumentAwaitingForAuthorTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML templates =<< (renderTemplate templates "mailDocumentAwaitingForAuthorContent" $ do
        field "authorname" $ BS.toString authorname
        field "documenttitle" $ BS.toString  documenttitle
        field "ctxhostpart" ctxhostpart
        field "documentlink" $ ctxhostpart ++ (show $ LinkIssueDoc documentid)
        field "partylist" signatories)
    return $ emptyMail {title = BS.fromString title, content = BS.fromString content}

mailCancelDocumentByAuthorContent :: KontrakcjaTemplates
                                  -> Bool
                                  -> (Maybe BS.ByteString)
                                  -> Context
                                  -> Document
                                  -> IO String
mailCancelDocumentByAuthorContent templates forMail customMessage ctx document = do
    renderTemplateForProcess templates document processmailcancelbyauthorcontent $ do
        field "header" $ do
            header <- case customMessage of
                           Just c -> return $ BS.toString c
                           Nothing -> renderTemplateForProcess templates document processmailcancelbyauthorstandardheader $ do
                               field "partylist" $ map (BS.toString . getSmartName) $ partyList document
            makeEditable templates "customtext" header
        field "footer" $ do
            if forMail
               then if isNothing customMessage
                       then mailFooter ctx document
                       else renderTemplate templates "customFooter" $
                                field "creatorname" creatorname
                    else do
                        this <- mailFooter ctx document
                        with <- renderTemplate templates "customFooter" $
                            field "creatorname" creatorname
                        replaceOnEdit templates this with
        field "creatorname" creatorname
        field "documenttitle" $ BS.toString $ documenttitle document
    where
        creatorname = BS.toString $ getSmartName $ fromJust $ getAuthorSigLink document

mailCancelDocumentByAuthor :: KontrakcjaTemplates
                           -> Maybe BS.ByteString
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> IO Mail
mailCancelDocumentByAuthor templates customMessage ctx document@Document{documenttitle} signlink = do
    title <- renderTemplate templates "mailCancelDocumentByAuthorTitle" $ do
        field "documenttitle" $ BS.toString documenttitle
    content <- wrapHTML templates =<< mailCancelDocumentByAuthorContent templates True customMessage ctx document
    return $ emptyMail { title = BS.fromString title, to = [MailAddress { fullname = getFullName signlink, email = getEmail signlink }], content = BS.fromString content }

mailMismatchSignatory :: Context
                        -> Document
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> String
                        -> Bool
                        -> IO Mail
mailMismatchSignatory ctx document authoremail authorname doclink signame badname msg isbad = do
    let Context { ctxtemplates } = ctx
    title <- renderTemplate ctxtemplates "mailMismatchSignatoryTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML ctxtemplates =<< (renderTemplate ctxtemplates "mailMismatchSignatoryContent" $ do
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

mailMismatchAuthor :: Context -> Document -> String -> String -> String -> IO Mail
mailMismatchAuthor ctx document authorname badname bademail = do
    let Context { ctxtemplates } = ctx
        Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    title <- renderTemplate ctxtemplates "mailMismatchAuthorTitle" $ do
        field "documenttitle" $ BS.toString $ documenttitle document
    content <- wrapHTML ctxtemplates =<< (renderTemplate ctxtemplates "mailMismatchAuthorContent" $ do
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

makeEditable :: KontrakcjaTemplates -> String -> String -> IO String
makeEditable templates name this =
    renderTemplate templates "makeEditable" $ do
        field "name" name
        field "this" this

replaceOnEdit :: KontrakcjaTemplates -> String -> String -> IO String
replaceOnEdit templates this with =
    renderTemplate templates "replaceOnEdit" $ do
        field "this" this
        field "with" with

mailFooter :: Context -> Document -> IO String
mailFooter ctx doc = do
    mservice <- liftMM (query . GetService) (return $ documentservice doc)
    case (documentmailfooter $ documentui doc) `mplus` (join $ servicemailfooter <$> serviceui <$> mservice) of
         Just footer -> return $ BS.toString footer
         Nothing -> renderTemplate (ctxtemplates ctx) "poweredBySkrivaPaPara" $ field "ctxhostpart" $ ctxhostpart ctx

makeFullLink :: Context -> Document -> String -> IO String
makeFullLink ctx doc link = do
    mservice <- liftMM (query . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ BS.toString location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link
