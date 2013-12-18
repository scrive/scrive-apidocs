module Doc.DocViewMail
    ( mailDocumentAwaitingForAuthor
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
    , brandingMailFields
    ) where

import Company.Model
import Company.CompanyUI
import Control.Logic
import Doc.DocInfo (getLastSignedTime)
import Doc.DocStateData
import Doc.DocUtils
import File.FileID
import File.File
import KontraLink
import MailContext (MailContextMonad(..), getMailContext, MailContext(..))
import Mails.SendMail
import MinutesTime (formatMinutesTime)
import Utils.Monoid
import Utils.Prelude
import Utils.Color
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
import BrandedDomains

-- FIXME: why do we even use that?
para :: String -> String
para s = "<p>" ++ s ++ "</p>"

mailDocumentRemind :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                   => Maybe String
                   -> SignatoryLink
                   -> Bool
                   -> Document
                   -> m Mail
mailDocumentRemind cm s ispreview d = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSigned True cm d s ispreview
  _                                       -> remindMailSigned    True cm d s ispreview

mailDocumentRemindContent :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                          => Maybe String
                          -> Document
                          -> SignatoryLink
                          -> Bool
                          -> m String
mailDocumentRemindContent cm d s ispreview = case s of
  SignatoryLink {maybesigninfo = Nothing} -> remindMailNotSignedContent False cm d s ispreview
  _                                       -> remindMailSignedContent False cm d s ispreview

remindMailNotSigned :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                    => Bool
                    -> Maybe String
                    -> Document
                    -> SignatoryLink
                    -> Bool
                    -> m Mail
remindMailNotSigned forMail customMessage document signlink ispreview = do
    mctx <- getMailContext
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document)
        authorname = getAuthorName document
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    documentMailWithDocLang document "remindMailNotSignedContract" $ do
        F.value  "custommessage" customMessage
        F.value  "authorname" authorname
        F.value "partners" $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $ map getSmartName $  filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
        F.value "someonesigned" $ not $ null $ filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "link" $ if forMail
          then makeFullLink mctx $ show $ LinkSignDoc document signlink
          else makeFullLink mctx "/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map filename authorattachmentfiles
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just signlink <| forMail |> Nothing) (mainfile)
        F.value "hassigattachments" $ not $ null $ concat $ signatoryattachments <$> documentsignatorylinks document
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "nojavascriptmagic" $ True
        F.value "javascriptmagic" $ False
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "ispreview" ispreview


remindMailSigned :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                 => Bool
                 -> Maybe String
                 -> Document
                 -> SignatoryLink
                 -> Bool
                 -> m Mail
remindMailSigned _forMail customMessage document signlink ispreview = do
    sheader <- remindMailSignedStandardHeader document signlink
    documentMailWithDocLang document "remindMailSigned" $ do
            F.valueM "header" $ makeEditable "customtext" $ fromMaybe sheader customMessage
            F.value "ispreview" ispreview


remindMailNotSignedContent :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                           => Bool
                           -> Maybe String
                           -> Document
                           -> SignatoryLink
                           -> Bool
                           -> m String
remindMailNotSignedContent forMail customMessage document signlink ispreview =
    content <$> remindMailNotSigned forMail customMessage document signlink ispreview

remindMailSignedContent :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                        => Bool
                        -> (Maybe String)
                        -> Document
                        -> SignatoryLink
                        -> Bool
                        -> m String
remindMailSignedContent forMail customMessage document signlink ispreview = do
    content <$> remindMailSigned forMail customMessage document signlink ispreview

remindMailSignedStandardHeader :: TemplatesMonad m
                               => Document
                               -> SignatoryLink
                               -> m String
remindMailSignedStandardHeader document signlink =
    renderLocalTemplate document "remindMailSignedStandardHeader" $ do
        F.value "documenttitle" $ documenttitle document
        F.value "author" $ getAuthorName document
        F.value "personname" $ getSmartName signlink

mailDocumentRejected :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                     => Maybe String
                     -> SignatoryLink
                     -> Bool
                     -> Document
                     -> m Mail
mailDocumentRejected customMessage rejector ispreview document = do
   documentMailWithDocLang document "mailRejectContractMail" $ do
        F.value "rejectorName" $ getSmartName rejector
        F.value "customMessage" $ customMessage
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "loginlink" $ show $ LinkIssueDoc $ documentid document
        F.value "ispreview" ispreview



mailDocumentRejectedContent :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                            => Maybe String
                            -> SignatoryLink
                            -> Bool
                            -> Document
                            -> m String
mailDocumentRejectedContent customMessage rejector ispreview document =
     content <$> mailDocumentRejected customMessage rejector ispreview document

mailDocumentErrorForAuthor :: (HasLang a, MonadDB m, TemplatesMonad m, MailContextMonad m) => a -> Document -> m Mail
mailDocumentErrorForAuthor authorlang document = do
   documentMail authorlang document "mailDocumentError" $ return ()

mailDocumentErrorForSignatory :: (MonadDB m, TemplatesMonad m, MailContextMonad m) => Document -> m Mail
mailDocumentErrorForSignatory document = do
   documentMailWithDocLang document "mailDocumentError" $ return ()


data InvitationTo = Sign | View
  deriving (Eq,Show)

fieldsInvitationTo :: TemplatesMonad m => InvitationTo -> Fields m ()
fieldsInvitationTo a = do
    F.value "sign" (a == Sign)
    F.value "view" (a == View)

mailInvitation :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
               => Bool
               -> InvitationTo
               -> Maybe SignatoryLink
               -> Bool
               -> Document
               -> m Mail
mailInvitation forMail
               invitationto
               msiglink
               ispreview
               document@Document{documentinvitetext, documenttitle } = do
    mctx <- getMailContext
    authorattachmentfiles <- mapM (dbQuery . GetFileByFileID . authorattachmentfile) (documentauthorattachments document)
    let creatorname = getSmartName $ fromJust $ getAuthorSigLink document
    let personname = maybe "" getSmartName msiglink
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document) -- There always should be main file but tests fail without it
    documentMailWithDocLang document "mailInvitationToSignContract" $ do
        fieldsInvitationTo invitationto
        F.value "nojavascriptmagic" $ forMail
        F.value "javascriptmagic" $ not forMail
        F.valueM "header" $ do
          defaultHeader <- if isSignatory msiglink || not forMail
                            then
                              renderLocalTemplate document "mailInvitationToSignContractDefaultHeader" $ do
                                F.value "creatorname" $ creatorname
                                F.value "personname" $ Just personname <| personname /= "" |> Nothing
                                F.value "documenttitle" $ documenttitle
                                F.value "hascustommessage" $ not $ null documentinvitetext
                            else
                              renderLocalTemplate document "mailInvitationToViewDefaultHeader" $ do
                                F.value "creatorname" creatorname
                                F.value "personname" personname
                                F.value "documenttitle" $ documenttitle
                                F.value "hascustommessage" $ not $ null documentinvitetext

          if null documentinvitetext then
            return defaultHeader
           else
            renderLocalTemplate document "mailInvitationCustomInvitationHeader" $ do
              F.value "defaultheader" defaultHeader
              F.value "creatorname" creatorname
              F.valueM "custommessage" $ makeEditable "customtext" documentinvitetext
        F.value "link" $ case msiglink of
          Just siglink -> makeFullLink mctx $ show (LinkSignDoc document siglink)
          Nothing -> makeFullLink mctx "/s/avsäkerhetsskälkanviendastvisalänkenfördinmotpart/"
        F.value "partners" $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $ map getSmartName $ filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
        F.value "someonesigned" $ not $ null $ filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map filename authorattachmentfiles
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (msiglink <| forMail |> Nothing) (mainfile)
        F.value "hassigattachments" $ length (concatMap signatoryattachments $ documentsignatorylinks document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "ispreview" ispreview


mailInvitationContent :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                      => Bool
                      -> InvitationTo
                      -> Maybe SignatoryLink
                      -> Bool
                      -> Document
                      -> m String
mailInvitationContent  forMail invitationto msiglink ispreview document = do
     content <$> mailInvitation forMail invitationto msiglink ispreview document

mailDocumentClosed :: (MonadDB m, TemplatesMonad m, MailContextMonad m) => Maybe KontraLink -> SignatoryLink -> Bool -> Document -> m Mail
mailDocumentClosed l sl sealFixed document = do
   mctx <- getMailContext
   partylist <- renderLocalListTemplate document $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
   let mainfile = fromMaybe (unsafeFileID 0) (documentsealedfile document)
   documentMailWithDocLang document "mailContractClosed" $ do
        F.value "partylist" $ partylist
        F.value "signatoryname" $ getSmartName sl
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "confirmationlink" $ (++) (mctxhostpart mctx) <$> show <$> l
        F.value "doclink" $ if isAuthor sl
                            then (++) (mctxhostpart mctx) $ show $ LinkIssueDoc (documentid document)
                            else (++) (mctxhostpart mctx) $ show $ LinkSignDoc document sl
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (Just sl) mainfile
        F.value "sealFixed" $ sealFixed
        F.value "closingtime" $ formatMinutesTime "%Y-%m-%d %H:%M %Z" $ getLastSignedTime document

mailDocumentAwaitingForAuthor :: (HasLang a, MonadDB m, TemplatesMonad m, MailContextMonad m) => a -> Document -> m Mail
mailDocumentAwaitingForAuthor authorlang document = do
    mctx <- getMailContext
    signatories <- renderLocalListTemplate authorlang $ map getSmartName $ filter (isSignatory &&^ (not . isAuthor)) (documentsignatorylinks document)
    signatoriesThatSigned <- renderLocalListTemplate authorlang $ map getSmartName $ filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
    let mainfile =  fromMaybe (unsafeFileID 0) (documentfile document) -- There always should be main file but tests fail without it
    documentMail authorlang document "mailDocumentAwaitingForAuthor" $ do
        F.value "authorname" $ getSmartName $ fromJust $ getAuthorSigLink document
        F.value "documentlink" $ (mctxhostpart mctx) ++ show (LinkSignDoc document $ fromJust $ getAuthorSigLink document)
        F.value "partylist" signatories
        F.value "partylistSigned" signatoriesThatSigned
        F.value "someonesigned" $ not $ null $ filter (isSignatory &&^ hasSigned) (documentsignatorylinks document)
        F.value "companyname" $ nothingIfEmpty $ getCompanyName document
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (getAuthorSigLink document) mainfile

mailMismatchSignatory :: (MonadDB m, TemplatesMonad m, MailContextMonad m)
                      => String
                      -> String
                      -> String
                      -> String
                      -> String
                      -> String
                      -> Bool
                      -> Document
                      -> m Mail
mailMismatchSignatory authoremail authorname doclink signame badname msg isbad document = do
   documentMailWithDocLang document "mailMismatchSignatory" $ do
        F.value "authorname" authorname
        F.value "signame" signame
        F.value "badname" badname
        F.value "authoremail" authoremail
        F.value "doclink" doclink
        F.value "messages" (if isbad then Just (concat $ map para $ lines msg) else Nothing)
        F.value "loginlink" $ show $ LinkLogin (getLang document) NotLogged

mailMismatchAuthor :: (HasLang a, MonadDB m, TemplatesMonad m, MailContextMonad m) => String -> [String] -> String -> String -> a -> Document -> m Mail
mailMismatchAuthor authorname badmessages badname bademail authorlang document = do
    documentMail authorlang document "mailMismatchAuthor" $ do
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

makeFullLink :: MailContext -> String -> String
makeFullLink mctx link = mctxhostpart mctx ++ link

documentMailWithDocLang :: (MonadDB m, TemplatesMonad m, MailContextMonad m) =>  Document -> String -> Fields m () -> m Mail
documentMailWithDocLang doc mailname otherfields = documentMail doc doc mailname otherfields

documentMail :: (HasLang a, MailContextMonad m, MonadDB m, TemplatesMonad m) =>  a -> Document -> String -> Fields m () -> m Mail
documentMail haslang doc mailname otherfields = do
    mctx <- getMailContext
    mcompany <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                   Just suid ->  fmap Just $ dbQuery $ GetCompanyByUserID $ suid
                   Nothing -> return Nothing
    mcompanyui <- case mcompany of
                    Just comp -> (dbQuery $ GetCompanyUI (companyid comp)) >>= return . Just
                    Nothing -> return Nothing
    let allfields = do
          F.value "ctxhostpart" (mctxhostpart mctx)
          F.value "ctxlang" (codeFromLang $ mctxlang mctx)
          F.value "documenttitle" $ documenttitle doc
          F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
          brandingMailFields (mctxcurrentBrandedDomain mctx) mcompanyui
          otherfields
    kontramaillocal (mctxmailsconfig mctx) (mctxcurrentBrandedDomain mctx) haslang mailname allfields

brandingMailFields :: Monad m => Maybe BrandedDomain -> Maybe CompanyUI -> Fields m ()
brandingMailFields mbd companyui = do
    F.value "background"  $ companyemailbackgroundcolour <$> companyui
    F.value "textcolor" $ (join $ companyemailtextcolour <$> companyui) `mplus`(bdmailstextcolor <$> mbd)
    F.value "font"  $ companyemailfont <$> companyui
    F.value "bordercolour"  $ companyemailbordercolour <$> companyui
    F.value "buttoncolour"  $ ensureHexRGB' <$> (join $ companyemailbuttoncolour <$> companyui) `mplus` (bdmailsbuttoncolor <$> mbd)
    F.value "emailbackgroundcolour"  $ (join $ companyemailemailbackgroundcolour <$> companyui) `mplus` (bdmailsbackgroundcolor <$> mbd)
    when (isJust companyui || isJust mbd) $ do
      F.value "logo" $ (isJust $ join $ companyemaillogo <$> companyui) || (isJust $ mbd)
      F.value "logoLink" $ if (isJust $ join $ companyemaillogo <$> companyui)
                              then (show <$> LinkCompanyEmailLogo <$> companyuicompanyid <$> companyui)
                              else (bdlogolink <$> mbd)
  where ensureHexRGB' s = fromMaybe s $ ensureHexRGB s
