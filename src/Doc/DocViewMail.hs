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
    , documentMailWithDocLang
    , companyBrandFields
    ) where

import Company.Model
import Control.Logic
import Doc.DocProcess
import Doc.DocStateData
import Doc.DocUtils
import File.FileID
import Kontra
import KontraLink
import Mails.SendMail
import Utils.Monad
import Utils.Monoid
import Utils.Prelude
import Text.StringTemplates.Templates
import Templates
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import DB
import Control.Monad
import Data.Functor
import Data.Maybe
import File.Model
import User.Model
import Util.HasSomeCompanyInfo
import qualified Text.StringTemplates.Fields as F

-- FIXME: why do we even use that?
para :: String -> String
para s = "<p>" ++ s ++ "</p>"

mailDocumentRemind :: (MonadDB m, TemplatesMonad m)
                   => Maybe String
                   -> Context
                   -> Document
                   -> SignatoryLink
                   -> Bool
                   -> m Mail
mailDocumentRemind cm c d s ispreview = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned True cm c d s ispreview
  _                                       -> remindMailSigned    True cm c d s ispreview

mailDocumentRemindContent :: (MonadDB m, TemplatesMonad m)
                          => Maybe String
                          -> Context
                          -> Document
                          -> SignatoryLink
                          -> Bool
                          -> m String
mailDocumentRemindContent cm c d s ispreview = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent False cm c d s ispreview
  _                                       -> remindMailSignedContent False cm c d s ispreview

remindMailNotSigned :: (MonadDB m, TemplatesMonad m)
                    => Bool
                    -> Maybe String
                    -> Context
                    -> Document
                    -> SignatoryLink
                    -> Bool
                    -> m Mail
remindMailNotSigned forMail customMessage ctx document signlink ispreview = do
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document)
        authorname = getAuthorName document
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    documentMailWithDocLang ctx document (fromMaybe "" $ getValueForProcess document processmailremindnotsigned) $ do
        F.value  "custommessage" customMessage
        F.value  "authorname" authorname
        F.value "partners" $ map getSmartName $ partyList document
        F.value "partnerswhosigned" $ map getSmartName $ partySignedList document
        F.value "someonesigned" $ not $ null $ partySignedList document
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "link" $ if forMail
          then makeFullLink ctx $ show $ LinkSignDoc document signlink
          else makeFullLink ctx "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
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
        F.value "ispreview" ispreview


remindMailSigned :: (MonadDB m, TemplatesMonad m)
                 => Bool
                 -> Maybe String
                 -> Context
                 -> Document
                 -> SignatoryLink
                 -> Bool
                 -> m Mail
remindMailSigned _forMail customMessage ctx document signlink ispreview = do
    sheader <- remindMailSignedStandardHeader document signlink
    documentMailWithDocLang ctx document "remindMailSigned" $ do
            F.valueM "header" $ makeEditable "customtext" $ fromMaybe sheader customMessage
            F.value "ispreview" ispreview


remindMailNotSignedContent :: (MonadDB m, TemplatesMonad m)
                           => Bool
                           -> Maybe String
                           -> Context
                           -> Document
                           -> SignatoryLink
                           -> Bool
                           -> m String
remindMailNotSignedContent forMail customMessage ctx document signlink ispreview =
    content <$> remindMailNotSigned forMail customMessage ctx document signlink ispreview

remindMailSignedContent :: (MonadDB m, TemplatesMonad m)
                        => Bool
                        -> (Maybe String)
                        -> Context
                        -> Document
                        -> SignatoryLink
                        -> Bool
                        -> m String
remindMailSignedContent forMail customMessage ctx document signlink ispreview = do
    content <$> remindMailSigned forMail customMessage ctx document signlink ispreview

remindMailSignedStandardHeader :: TemplatesMonad m
                               => Document
                               -> SignatoryLink
                               -> m String
remindMailSignedStandardHeader document signlink =
    renderLocalTemplate document "remindMailSignedStandardHeader" $ do
        F.value "documenttitle" $ documenttitle document
        F.value "author" $ getAuthorName document
        F.value "personname" $ getSmartName signlink

mailDocumentRejected :: (MonadDB m, TemplatesMonad m)
                     => Maybe String
                     -> Context
                     -> Document
                     -> SignatoryLink
                     -> Bool
                     -> m Mail
mailDocumentRejected customMessage ctx document rejector ispreview = do
   documentMailWithDocLang ctx document (fromMaybe "" $ getValueForProcess document processmailreject) $ do
        F.value "rejectorName" $ getSmartName rejector
        F.value "customMessage" $ customMessage
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "loginlink" $ show $ LinkLogin (getLang document) NotLogged
        F.value "ispreview" ispreview



mailDocumentRejectedContent :: (MonadDB m, TemplatesMonad m)
                            => Maybe String
                            -> Context
                            -> Document
                            -> SignatoryLink
                            -> Bool
                            -> m String
mailDocumentRejectedContent customMessage ctx  document rejector ispreview =
     content <$> mailDocumentRejected customMessage ctx document rejector ispreview

mailDocumentErrorForAuthor :: (HasLang a, MonadDB m, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentErrorForAuthor ctx document authorlang = do
   documentMail authorlang ctx document "mailDocumentError" $ return ()

mailDocumentErrorForSignatory :: (MonadDB m, TemplatesMonad m) => Context -> Document -> m Mail
mailDocumentErrorForSignatory ctx document = do
   documentMailWithDocLang ctx document "mailDocumentError" $ return ()


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
               -> Bool
               -> m Mail
mailInvitation forMail
               ctx
               invitationto
               document@Document{documentinvitetext, documenttitle }
               msiglink
               ispreview = do
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    let creatorname = getSmartName $ fromJust $ getAuthorSigLink document
    let personname = maybe "" getSmartName msiglink
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document) -- There always should be main file but tests fail without it
    documentMailWithDocLang ctx document (fromMaybe "" $ getValueForProcess document processmailinvitationtosign) $ do
        fieldsInvitationTo invitationto
        F.value "nojavascriptmagic" $ forMail
        F.value "javascriptmagic" $ not forMail
        F.valueM "header" $ do
          defaultHeader <- if isSignatory msiglink || not forMail then
                            renderLocalTemplateForProcess document processmailinvitationtosigndefaultheader $ do
                              F.value "creatorname" $ creatorname
                              F.value "personname" $ Just personname <| personname /= "" |> Nothing
                              F.value "documenttitle" $ documenttitle
                          else
                            renderLocalTemplate document "mailInvitationToViewDefaultHeader" $ do
                              F.value "creatorname" creatorname
                              F.value "personname" personname
                              F.value "documenttitle" $ documenttitle
          if null documentinvitetext then
            return defaultHeader
           else
            renderLocalTemplate document "mailInvitationCustomInvitationHeader" $ do
              F.value "defaultheader" defaultHeader
              F.value "creatorname" creatorname
              F.valueM "custommessage" $ makeEditable "customtext" documentinvitetext
        F.value "link" $ case msiglink of
          Just siglink -> makeFullLink ctx $ show (LinkSignDoc document siglink)
          Nothing -> makeFullLink ctx "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
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
        F.value "ispreview" ispreview


mailInvitationContent :: (MonadDB m, TemplatesMonad m)
                      => Bool
                      -> Context
                      -> InvitationTo
                      -> Document
                      -> Maybe SignatoryLink
                      -> Bool
                      -> m String
mailInvitationContent  forMail ctx invitationto document msiglink ispreview = do
     content <$> mailInvitation forMail ctx invitationto document msiglink ispreview

mailDocumentClosed :: (MonadDB m, TemplatesMonad m) => Context -> Document -> Maybe KontraLink -> SignatoryLink -> m Mail
mailDocumentClosed ctx document l sl = do
   partylist <- renderLocalListTemplate document $ map getSmartName $ partyList document
   let mainfile = fromMaybe (unsafeFileID 0) (documentsealedfile document)
   documentMailWithDocLang ctx document (fromMaybe "" $ getValueForProcess document processmailclosed) $ do
        F.value "partylist" $ partylist
        F.value "signatoryname" $ getSmartName sl
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "confirmationlink" $ (++) (ctxhostpart ctx) <$> show <$> l
        F.value "doclink" $ if isAuthor sl
                            then (++) (ctxhostpart ctx) $ show $ LinkIssueDoc (documentid document)
                            else (++) (ctxhostpart ctx) $ show $ LinkSignDoc document sl
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just sl) mainfile

mailDocumentAwaitingForAuthor :: (HasLang a, MonadDB m, TemplatesMonad m) => Context -> Document -> a -> m Mail
mailDocumentAwaitingForAuthor ctx document authorlang = do
    signatories <- renderLocalListTemplate authorlang $ map getSmartName $ partySignedList document
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document) -- There always should be main file but tests fail without it
    documentMail authorlang ctx document "mailDocumentAwaitingForAuthor" $ do
        F.value "authorname" $ getSmartName $ fromJust $ getAuthorSigLink document
        F.value "documentlink" $ (ctxhostpart ctx) ++ show (LinkSignDoc document $ fromJust $ getAuthorSigLink document)
        F.value "partylist" signatories
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (getAuthorSigLink document) mainfile

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
   documentMailWithDocLang ctx document "mailMismatchSignatory" $ do
        F.value "authorname" authorname
        F.value "signame" signame
        F.value "badname" badname
        F.value "authoremail" authoremail
        F.value "doclink" doclink
        F.value "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing)
        F.value "loginlink" $ show $ LinkLogin (getLang document) NotLogged

mailMismatchAuthor :: (HasLang a, MonadDB m, TemplatesMonad m) => Context -> Document -> String -> [String] -> String -> String -> a -> m Mail
mailMismatchAuthor ctx document authorname badmessages badname bademail authorlang = do
    documentMail authorlang ctx document "mailMismatchAuthor" $ do
        F.value "messages" $ concat $ map para $ badmessages
        F.value "authorname" authorname
        F.value "bademail" bademail
        F.value "badname" badname
        F.value "loginlink" $ show $ LinkLogin (getLang authorlang) NotLogged

-- helpers

makeEditable :: TemplatesMonad m => String -> String -> m String
makeEditable name this = renderTemplate "makeEditable" $ do
  F.value "name" name
  F.value "this" this

makeFullLink :: Context -> String -> String
makeFullLink ctx link = ctxhostpart ctx ++ link

documentMailWithDocLang :: (MonadDB m, TemplatesMonad m) => Context -> Document -> String -> Fields m () -> m Mail
documentMailWithDocLang ctx doc mailname otherfields = documentMail doc ctx doc mailname otherfields

documentMail :: (HasLang a, MonadDB m, TemplatesMonad m) =>  a -> Context -> Document -> String -> Fields m () -> m Mail
documentMail haslang ctx doc mailname otherfields = do
    mcompany <- liftMM (dbQuery . GetCompanyByUserID) (return $ getAuthorSigLink doc >>= maybesignatory)
    let allfields = do
        F.value "ctxhostpart" (ctxhostpart ctx)
        F.value "ctxlang" (codeFromLang $ ctxlang ctx)
        F.value "documenttitle" $ documenttitle doc
        F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
        when (isJust mcompany) $ do
            let (Just company) = mcompany
            F.object "companybrand" $ companyBrandFields company
        otherfields
    kontramaillocal haslang mailname allfields

companyBrandFields :: Monad m => Company -> Fields m ()
companyBrandFields company = do
  F.value "background"  $ companyemailbackgroundcolour $ companyui company
  F.value "textcolor" $ companyemailtextcolour $ companyui company
  F.value "font"  $ companyemailfont $ companyui company
  F.value "bordercolour"  $ companyemailbordercolour $ companyui company
  F.value "buttoncolour"  $ companyemailbuttoncolour $ companyui company
  F.value "emailbackgroundcolour"  $ companyemailemailbackgroundcolour $ companyui company
  F.value "logo" $ isJust $ companyemaillogo $ companyui company
  F.value "logoLink" $ show $ LinkCompanyEmailLogo $ companyid company
