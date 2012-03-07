module Doc.DocViewMail (
      mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentErrorForAuthor
    , mailDocumentErrorForSignatory
    , mailDocumentRejected
    , mailDocumentRejectedContent
    , mailDocumentRemind
    , mailDocumentRemindContent
    , InvitationTo(..)
    , mailInvitation
    , mailInvitationContent
    , mailMismatchAuthor
    , mailMismatchSignatory
    , documentMailWithDocLocale
    , mailFooterForDocument
    , mailFooterForUser
    , companyBrandFields
    ) where

import API.Service.Model
import Company.Model
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
import File.Model
import Control.Monad.Trans
import AppView
import User.Model
import Util.HasSomeCompanyInfo

mailDocumentRemind :: TemplatesMonad m
                   => Maybe String
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> m Mail
mailDocumentRemind cm c d s =
    case s of
         SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned True cm c d s
         _                                       -> remindMailSigned    True cm c d s

mailDocumentRemindContent :: TemplatesMonad m
                          => Maybe String
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
                    -> Maybe String
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> m Mail
remindMailNotSigned forMail customMessage ctx document signlink = do
    let mainfile =  head $ (documentfiles document) ++ [unsafeFileID 0]
    authorattachmentfiles <- mapM (ioRunDB (ctxdbenv ctx) . dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailremindnotsigned) $ do
        fieldM "header" $ do
            header <- if isNothing customMessage
                         then remindMailNotSignedStandardHeader document signlink
                         else return $ fromJust customMessage
            makeEditable "customtext" header
        fieldM "footer" $ mailFooterForDocument ctx document
        field "partners" $ map getSmartName $ partyList document
        field "partnerswhosigned" $ map getSmartName $ partySignedList document
        field "someonesigned" $ not $ null $ partySignedList document
        field "timetosign" $ show <$> documenttimeouttime document
        fieldM "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        field "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just signlink <| forMail |> Nothing) (mainfile)
        field "hassigattachments" $ not $ null $ concat $ signatoryattachments <$> documentsignatorylinks document
        -- We try to use generic templates and this is why we return a tuple
        field "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        field "nojavascriptmagic" $ True
        field "javascriptmagic" $ False
        field "companyname" $ nothingIfEmpty $ getCompanyName document


remindMailSigned :: TemplatesMonad m
                 => Bool
                 -> Maybe String
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> m Mail
remindMailSigned _forMail customMessage ctx document signlink = do
    sheader <- remindMailSignedStandardHeader document signlink
    documentMailWithDocLocale ctx document "remindMailSigned" $ do
            fieldM "header" $ makeEditable "customtext" $ fromMaybe sheader customMessage
            fieldM "footer" $ mailFooterForDocument ctx document


remindMailNotSignedContent :: TemplatesMonad m
                           => Bool
                           -> Maybe String
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink =
    content <$> remindMailNotSigned forMail customMessage ctx document signlink

remindMailSignedContent :: TemplatesMonad m
                        => Bool
                        -> (Maybe String)
                        -> Context
                        -> Document
                        -> SignatoryLink
                        -> m String
remindMailSignedContent forMail customMessage ctx document signlink = do
    content <$> remindMailSigned forMail customMessage ctx document signlink

remindMailSignedStandardHeader :: TemplatesMonad m
                               => Document
                               -> SignatoryLink
                               -> m String
remindMailSignedStandardHeader document signlink =
    renderLocalTemplateForProcess document processmailsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe "" getSmartName mauthorsiglink
        field "documenttitle" $ documenttitle document
        field "author" creatorname
        field "personname" $ getSmartName signlink
        field "service" $ isJust $ documentservice document

remindMailNotSignedStandardHeader :: TemplatesMonad m
                                  => Document
                                  -> SignatoryLink
                                  -> m String
remindMailNotSignedStandardHeader document signlink =
    renderLocalTemplateForProcess document processmailnotsignedstandardheader $ do
        let mauthorsiglink = getAuthorSigLink document
            creatorname = maybe "" getSmartName mauthorsiglink
        field "documenttitle" $ documenttitle document
        field "author" $ creatorname
        field "personname" $ getSmartName signlink
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
        fieldM "footer" $ mailFooterForUser ctx document
        field "customMessage" $ customMessage
        field "companyname" $ nothingIfEmpty $ getCompanyName document



mailDocumentRejectedContent :: TemplatesMonad m
                      => Maybe String
                      -> Context
                      -> Document
                      -> SignatoryLink
                      -> m String
mailDocumentRejectedContent customMessage ctx  document rejector =
     content <$> mailDocumentRejected customMessage ctx document rejector

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
    authorattachmentfiles <- mapM (ioRunDB (ctxdbenv ctx) . dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    let creatorname = getSmartName $ fromJust $ getAuthorSigLink document
    let issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
    let personname = maybe "" getSmartName msiglink
    let mainfile =  head $ (documentfiles document) ++ [unsafeFileID 0] -- There always should be main file but tests fail without it
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailinvitationtosign) $ do
        fieldsInvitationTo invitationto
        field "nojavascriptmagic" $ forMail
        field "javascriptmagic" $ not forMail
        fieldM "header" $ do
            header <- if null documentinvitetext
                         then if issignatory || not forMail
                                 then renderLocalTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                                     field "creatorname" $ creatorname
                                     field "personname" $ personname
                                     field "documenttitle" $ documenttitle
                                     field "service" $ isJust $ documentservice document
                                 else renderLocalTemplateFM document "mailInvitationToViewDefaultHeader" $ do
                                     field "creatorname" creatorname
                                     field "personname" personname
                                     field "documenttitle" $ documenttitle
                                     field "service" $ isJust $ documentservice document
                         else return documentinvitetext
            makeEditable "customtext" header
        fieldM "footer" $ mailFooterForDocument ctx document
        fieldM "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        field "partners" $ map getSmartName $ partyList document
        field "partnerswhosigned" $ map getSmartName $ partySignedList document
        field "someonesigned" $ not $ null $ partySignedList document
        field "timetosign" $ show <$> documenttimeouttime document
        field "isattachments" $ length (documentauthorattachments document) > 0
        field "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        field "previewLink" $ show $ LinkDocumentPreview (documentid document) (msiglink <| forMail |> Nothing) (mainfile)
        field "hassigattachments" $ length (concatMap signatoryattachments $ documentsignatorylinks document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        field "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        field "companyname" $ nothingIfEmpty $ getCompanyName document


mailInvitationContent :: TemplatesMonad m
                     => Bool
                     -> Context
                     -> InvitationTo
                     -> Document
                     -> Maybe SignatoryLink
                     -> m String
mailInvitationContent  forMail ctx invitationto document msiglink = do
     content <$> mailInvitation forMail ctx invitationto document msiglink

mailDocumentClosed :: TemplatesMonad m => Context -> Document -> m Mail
mailDocumentClosed ctx document= do
   partylist <- renderLocalListTemplate document $ map getSmartName $ partyList document
   documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailclosed) $ do
        field "partylist" $ partylist
        fieldM "footer" $ mailFooterForDocument ctx document
        field "companyname" $ nothingIfEmpty $ getCompanyName document


mailDocumentAwaitingForAuthor :: (HasLocale a, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentAwaitingForAuthor ctx document authorlocale = do
    signatories <- renderLocalListTemplate authorlocale $ map getSmartName $ partySignedList document
    documentMail authorlocale ctx document "mailDocumentAwaitingForAuthor" $ do
        field "authorname" $ getSmartName $ fromJust $ getAuthorSigLink document
        field "documentlink" $ (ctxhostpart ctx) ++ show (LinkSignDoc document $ fromJust $ getAuthorSigLink document)
        field "partylist" signatories
        field "companyname" $ nothingIfEmpty $ getCompanyName document

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
        field "doclink" $ ctxhostpart ctx ++ (show $ LinkDesignDoc (documentid document))
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
mailFooterForDocument :: TemplatesMonad m => Context -> Document -> m String
mailFooterForDocument ctx doc =
  firstWithDefault  [   getDocumentFooter doc
                      , getUserFooter ctx
                      , getServiceFooter ctx doc
                   ] (defaultFooter ctx)


{- |
    Use for anyone sending a mail as a person invited to that document.  For example when rejecting
    a document.
    The footer is in order of preference:
      1. a custom footer configured on the context user (if there is one)
      3. the default powered by scrive footer
-}
mailFooterForUser :: TemplatesMonad m => Context -> Document -> m String
mailFooterForUser ctx doc =
  firstWithDefault [   getUserFooter ctx
                     , getServiceFooter ctx doc
                   ] (defaultFooter ctx)

getUserFooter :: Monad m => Context -> m (Maybe String)
getUserFooter ctx = return $ join $ customfooter <$> usersettings <$> ctxmaybeuser ctx

getDocumentFooter :: MonadIO m => Document -> m (Maybe String)
getDocumentFooter doc = return $ documentmailfooter $ documentui doc

getServiceFooter :: MonadIO m => Context -> Document -> m (Maybe String)
getServiceFooter ctx doc = do
  mservice <- liftMM (ioRunDB (ctxdbenv ctx) . dbQuery . GetService) (return $ documentservice doc)
  return $ mservice >>= servicemailfooter . serviceui

defaultFooter :: TemplatesMonad m => Context -> m String
defaultFooter ctx = renderTemplateM "poweredByScrive" $ [("ctxhostpart", ctxhostpart ctx)]

makeFullLink :: TemplatesMonad m => Context -> Document -> String -> m String
makeFullLink ctx doc link = do
    mservice <- liftMM (ioRunDB (ctxdbenv ctx) . dbQuery . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link


documentMailWithDocLocale :: (MonadIO m,Functor m,TemplatesMonad m) => Context -> Document -> String -> Fields m -> m Mail
documentMailWithDocLocale ctx doc mailname otherfields = documentMail doc ctx doc mailname otherfields

documentMail :: (MonadIO m,Functor m,TemplatesMonad m, HasLocale a) =>  a -> Context -> Document -> String -> Fields m -> m Mail
documentMail haslocale ctx doc mailname otherfields = do
    mservice <- liftMM (ioRunDB (ctxdbenv ctx) . dbQuery . GetService) (return $ documentservice doc)
    mcompany <- liftMM (ioRunDB (ctxdbenv ctx) . dbQuery . GetCompany) (return $ getAuthorSigLink doc >>= maybecompany)
    let allfields = do
        contextFields ctx
        field "documenttitle" $ documenttitle doc
        field "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
        when (isJust mcompany) $ do
            let (Just company) = mcompany
            fieldF "companybrand" $ companyBrandFields company
        when (isJust mservice) $
            fieldF "service" $ serviceFields "" mservice
        otherfields
    kontramaillocal haslocale mailname allfields

companyBrandFields :: MonadIO m => Company -> Fields m
companyBrandFields company = do
    field "barsbackground"  $ companybarsbackground $ companyui company
    field "barstextcolour" $ companybarstextcolour $ companyui company
    field "logo" $ isJust $ companylogo $ companyui company
    field "logoLink"  $ show $ LinkCompanyLogo $ companyid company
