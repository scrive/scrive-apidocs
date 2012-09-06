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
import Control.Logic
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocUtils
import File.FileID
import Kontra
import KontraLink
import Mails.SendMail
import Utils.List
import Utils.Monad
import Utils.Monoid
import Utils.Prelude
import Templates.Templates
import Templates.TemplatesUtils
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import DB
import Control.Monad
import Data.Functor
import Data.Maybe
import File.Model
import AppView
import User.Model
import Util.HasSomeCompanyInfo
import qualified Templates.Fields as F

-- FIXME: why do we even use that?
para :: String -> String
para s = "<p>" ++ s ++ "</p>"

mailDocumentRemind :: (MonadDB m, TemplatesMonad m)
                   => Maybe String
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> m Mail
mailDocumentRemind cm c d s = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned True cm c d s
  _                                       -> remindMailSigned    True cm c d s

mailDocumentRemindContent :: (MonadDB m, TemplatesMonad m)
                          => Maybe String
                          -> Context
                          -> Document
                          -> SignatoryLink
                          -> m String
mailDocumentRemindContent cm c d s = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s
  _                                       -> remindMailSignedContent False cm c d s

remindMailNotSigned :: (MonadDB m, TemplatesMonad m)
                    => Bool
                    -> Maybe String
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> m Mail
remindMailNotSigned forMail customMessage ctx document signlink = do
    let mainfile =  head $ (documentfiles document) ++ [unsafeFileID 0]
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailremindnotsigned) $ do
        F.valueM "header" $ do
            header <- if isNothing customMessage
                         then remindMailNotSignedStandardHeader document signlink
                         else return $ fromJust customMessage
            makeEditable "customtext" header
        F.valueM "footer" $ mailFooterForDocument ctx document
        F.value "partners" $ map getSmartName $ partyList document
        F.value "partnerswhosigned" $ map getSmartName $ partySignedList document
        F.value "someonesigned" $ not $ null $ partySignedList document
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.valueM "link" $ do
            if forMail
               then makeFullLink ctx document $ show $ LinkSignDoc document signlink
               else makeFullLink ctx document $ "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just signlink <| forMail |> Nothing) (mainfile)
        F.value "hassigattachments" $ not $ null $ concat $ signatoryattachments <$> documentsignatorylinks document
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "nojavascriptmagic" $ True
        F.value "javascriptmagic" $ False
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document


remindMailSigned :: (MonadDB m, TemplatesMonad m)
                 => Bool
                 -> Maybe String
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> m Mail
remindMailSigned _forMail customMessage ctx document signlink = do
    sheader <- remindMailSignedStandardHeader document signlink
    documentMailWithDocLocale ctx document "remindMailSigned" $ do
            F.valueM "header" $ makeEditable "customtext" $ fromMaybe sheader customMessage
            F.valueM "footer" $ mailFooterForDocument ctx document


remindMailNotSignedContent :: (MonadDB m, TemplatesMonad m)
                           => Bool
                           -> Maybe String
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink =
    content <$> remindMailNotSigned forMail customMessage ctx document signlink

remindMailSignedContent :: (MonadDB m, TemplatesMonad m)
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
        F.value "documenttitle" $ documenttitle document
        F.value "author" $ getAuthorName document
        F.value "personname" $ getSmartName signlink
        F.value "service" $ isJust $ documentservice document

remindMailNotSignedStandardHeader :: TemplatesMonad m
                                  => Document
                                  -> SignatoryLink
                                  -> m String
remindMailNotSignedStandardHeader document signlink =
    renderLocalTemplateForProcess document processmailnotsignedstandardheader $ do
        F.value "documenttitle" $ documenttitle document
        F.value "author" $ getAuthorName document
        F.value "personname" $ getSmartName signlink
        F.value "service" $ isJust $ documentservice document

mailDocumentRejected :: (MonadDB m, TemplatesMonad m)
                     => Maybe String
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> m Mail
mailDocumentRejected customMessage ctx document rejector = do
   documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailreject) $ do
        F.value "rejectorName" $ getSmartName rejector
        F.valueM "footer" $ mailFooterForUser ctx document
        F.value "customMessage" $ customMessage
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document



mailDocumentRejectedContent :: (MonadDB m, TemplatesMonad m)
                            => Maybe String
                            -> Context
                            -> Document
                            -> SignatoryLink
                            -> m String
mailDocumentRejectedContent customMessage ctx  document rejector =
     content <$> mailDocumentRejected customMessage ctx document rejector

mailDocumentErrorForAuthor :: (HasLocale a, MonadDB m, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentErrorForAuthor ctx document authorlocale = do
   documentMail authorlocale ctx document "mailDocumentError" $ return ()

mailDocumentErrorForSignatory :: (MonadDB m, TemplatesMonad m) => Context -> Document -> m Mail
mailDocumentErrorForSignatory ctx document = do
   documentMailWithDocLocale ctx document "mailDocumentError" $ return ()


data InvitationTo = Sign | View
  deriving (Eq,Show)

fieldsInvitationTo :: TemplatesMonad m => InvitationTo -> Fields m ()
fieldsInvitationTo a = do
    F.value "sign" (a == Sign)
    F.value "view" (a == View)

mailInvitation :: (MonadDB m, TemplatesMonad m)
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
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    let creatorname = getSmartName $ fromJust $ getAuthorSigLink document
    let issignatory = maybe False (elem SignatoryPartner . signatoryroles) msiglink
    let personname = maybe "" getSmartName msiglink
    let mainfile =  head $ (documentfiles document) ++ [unsafeFileID 0] -- There always should be main file but tests fail without it
    documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailinvitationtosign) $ do
        fieldsInvitationTo invitationto
        F.value "nojavascriptmagic" $ forMail
        F.value "javascriptmagic" $ not forMail
        F.valueM "header" $ do
            header <- if null documentinvitetext
                         then if issignatory || not forMail
                                 then renderLocalTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                                     F.value "creatorname" $ creatorname
                                     F.value "personname" $ personname
                                     F.value "documenttitle" $ documenttitle
                                     F.value "service" $ isJust $ documentservice document
                                 else renderLocalTemplate document "mailInvitationToViewDefaultHeader" $ do
                                     F.value "creatorname" creatorname
                                     F.value "personname" personname
                                     F.value "documenttitle" $ documenttitle
                                     F.value "service" $ isJust $ documentservice document
                         else return documentinvitetext
            makeEditable "customtext" header
        F.valueM "footer" $ mailFooterForDocument ctx document
        F.valueM "link" $ do
            case msiglink of
                 Just siglink -> makeFullLink ctx document $ show (LinkSignDoc document siglink)
                 Nothing -> makeFullLink ctx document "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        F.value "partners" $ map getSmartName $ partyList document
        F.value "partnerswhosigned" $ map getSmartName $ partySignedList document
        F.value "someonesigned" $ not $ null $ partySignedList document
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map (filename) (catMaybes authorattachmentfiles)
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (msiglink <| forMail |> Nothing) (mainfile)
        F.value "hassigattachments" $ length (concatMap signatoryattachments $ documentsignatorylinks document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document


mailInvitationContent :: (MonadDB m, TemplatesMonad m)
                      => Bool
                      -> Context
                      -> InvitationTo
                      -> Document
                      -> Maybe SignatoryLink
                      -> m String
mailInvitationContent  forMail ctx invitationto document msiglink = do
     content <$> mailInvitation forMail ctx invitationto document msiglink

mailDocumentClosed :: (MonadDB m, TemplatesMonad m) => Context -> Document -> m Mail
mailDocumentClosed ctx document= do
   partylist <- renderLocalListTemplate document $ map getSmartName $ partyList document
   documentMailWithDocLocale ctx document (fromMaybe "" $ getValueForProcess document processmailclosed) $ do
        F.value "partylist" $ partylist
        F.valueM "footer" $ mailFooterForDocument ctx document
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document


mailDocumentAwaitingForAuthor :: (HasLocale a, MonadDB m, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentAwaitingForAuthor ctx document authorlocale = do
    signatories <- renderLocalListTemplate authorlocale $ map getSmartName $ partySignedList document
    documentMail authorlocale ctx document "mailDocumentAwaitingForAuthor" $ do
        F.value "authorname" $ getSmartName $ fromJust $ getAuthorSigLink document
        F.value "documentlink" $ (ctxhostpart ctx) ++ show (LinkSignDoc document $ fromJust $ getAuthorSigLink document)
        F.value "partylist" signatories
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.valueM "footer" $ mailFooterForDocument ctx document

mailMismatchSignatory :: (MonadDB m, TemplatesMonad m)
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
        F.value "authorname" authorname
        F.value "signame" signame
        F.value "badname" badname
        F.value "authoremail" authoremail
        F.value "doclink" doclink
        F.value "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing)

mailMismatchAuthor :: (HasLocale a, MonadDB m, TemplatesMonad m) => Context -> Document -> String -> String -> String -> a -> m Mail
mailMismatchAuthor ctx document authorname badname bademail authorlocale = do
    let Just (ELegDataMismatch msg _ _ _ _) = documentcancelationreason document
    documentMail authorlocale ctx document "mailMismatchAuthor" $ do
        F.value "messages" $ concat $ map para $ lines msg
        F.value "authorname" authorname
        F.value "doclink" $ ctxhostpart ctx ++ (show $ LinkDesignDoc (documentid document))
        F.value "bademail" bademail
        F.value "badname" badname

-- helpers

makeEditable :: TemplatesMonad m => String -> String -> m String
makeEditable name this = renderTemplate "makeEditable" $ do
  F.value "name" name
  F.value "this" this

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
mailFooterForDocument :: (MonadDB m, TemplatesMonad m) => Context -> Document -> m (Maybe String)
mailFooterForDocument ctx doc = firstOrNothing  [
    getDocumentFooter doc
  , getUserFooter ctx
  , getServiceFooter doc
  ]


{- |
    Use for anyone sending a mail as a person invited to that document.  For example when rejecting
    a document.
    The footer is in order of preference:
      1. a custom footer configured on the context user (if there is one)
      3. the default powered by scrive footer
-}
mailFooterForUser :: (MonadDB m, TemplatesMonad m) => Context -> Document -> m String
mailFooterForUser ctx doc = firstWithDefault [
    getUserFooter ctx
  , getServiceFooter doc
  ] (defaultFooter ctx)

getUserFooter :: Monad m => Context -> m (Maybe String)
getUserFooter ctx = return $ join $ customfooter <$> usersettings <$> ctxmaybeuser ctx

getDocumentFooter :: Monad m => Document -> m (Maybe String)
getDocumentFooter doc = return $ documentmailfooter $ documentui doc

getServiceFooter :: MonadDB m => Document -> m (Maybe String)
getServiceFooter doc = do
  mservice <- liftMM (dbQuery . GetService) (return $ documentservice doc)
  return $ mservice >>= servicemailfooter . serviceui

defaultFooter :: TemplatesMonad m => Context -> m String
defaultFooter ctx = renderTemplate "poweredByScrive" $ do
  F.value "ctxhostpart" $ ctxhostpart ctx

makeFullLink :: (MonadDB m, TemplatesMonad m) => Context -> Document -> String -> m String
makeFullLink ctx doc link = do
    mservice <- liftMM (dbQuery . GetService) (return $ documentservice doc)
    case join $ servicelocation <$> servicesettings <$> mservice of
         Just (ServiceLocation location) -> return $ location ++ link
         Nothing -> return $ ctxhostpart ctx ++ link


documentMailWithDocLocale :: (MonadDB m, TemplatesMonad m) => Context -> Document -> String -> Fields m () -> m Mail
documentMailWithDocLocale ctx doc mailname otherfields = documentMail doc ctx doc mailname otherfields

documentMail :: (HasLocale a, MonadDB m, TemplatesMonad m) =>  a -> Context -> Document -> String -> Fields m () -> m Mail
documentMail haslocale ctx doc mailname otherfields = do
    mservice <- liftMM (dbQuery . GetService) (return $ documentservice doc)
    mcompany <- liftMM (dbQuery . GetCompany) (return $ getAuthorSigLink doc >>= maybecompany)
    let allfields = do
        contextFields ctx
        F.value "documenttitle" $ documenttitle doc
        F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
        when (isJust mcompany) $ do
            let (Just company) = mcompany
            F.object "companybrand" $ companyBrandFields company
        when (isJust mservice) $
            F.object "service" $ serviceFields "" mservice
        otherfields
    kontramaillocal haslocale mailname allfields

companyBrandFields :: Monad m => Company -> Fields m ()
companyBrandFields company = do
  F.value "barsbackground"  $ companybarsbackground $ companyui company
  F.value "barstextcolour" $ companybarstextcolour $ companyui company
  F.value "logo" $ isJust $ companylogo $ companyui company
  F.value "logoLink" $ show $ LinkCompanyLogo $ companyid company
