module Doc.DocViewMail
    ( mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentErrorForAuthor
    , mailDocumentErrorForSignatory
    , mailDocumentRejected
    , mailDocumentRemind
    , mailDocumentRemindContent
    , mailForwardSigned
    , InvitationTo(..)
    , mailInvitation
    , mailInvitationContent
    , mailClosedContent
    , documentMailWithDocLang
    , brandingMailFields
    ) where

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch
import Control.Monad.Trans (lift)
import Data.Functor
import Text.StringTemplates.Templates
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Branding.Adler32
import Company.CompanyUI
import Company.Model
import DB
import Doc.DocInfo (getLastSignedTime)
import Doc.DocStateData
import Doc.DocUtils
import Doc.Model (unsavedDocumentLingerDays)
import File.FileID
import KontraLink
import KontraPrelude
import MailContext
import Mails.SendMail
import MinutesTime
import Templates
import Theme.Model
import User.Model
import Util.HasSomeCompanyInfo
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Color
import Utils.Monoid

mailDocumentRemind :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m)
                   => Bool
                   -> Maybe String
                   -> SignatoryLink
                   -> Bool
                   -> Document
                   -> m Mail
mailDocumentRemind automatic cm s documentAttached d = case (d, s) of
  (_, SignatoryLink {maybesigninfo = Nothing, signatoryispartner = True}) -> remindMailNotSigned automatic True cm d s
  (Document {documentstatus = Pending}, SignatoryLink {signatoryispartner = False}) -> remindMailNotSigned automatic True cm d s
  _                                       -> remindMailSigned    True cm d s documentAttached

mailDocumentRemindContent :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m)
                          => Maybe String -> Document -> SignatoryLink -> Bool -> m String
mailDocumentRemindContent cm d s documentAttached = content <$> case (d, s) of
  (_, SignatoryLink {maybesigninfo = Nothing, signatoryispartner = True}) -> remindMailNotSigned False False cm d s
  (Document {documentstatus = Pending}, SignatoryLink {signatoryispartner = False}) -> remindMailNotSigned False False cm d s
  _                                       -> remindMailSigned    False cm d s documentAttached

remindMailNotSigned :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
                    => Bool
                    -> Bool
                    -> Maybe String
                    -> Document
                    -> SignatoryLink
                    -> m Mail
remindMailNotSigned automatic forMail customMessage document signlink = do
    mctx <- getMailContext
    let mainfile =  fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
        authorname = getAuthorName document
    documentMailWithDocLang document (templateName "remindMailNotSignedContract") $ do
        F.value "sign" $ signatoryispartner signlink
        F.value  "custommessage" $ asCustomMessage <$> customMessage
        F.value  "authorname" authorname
        F.value  "automatic" automatic
        F.value "partners" $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $ map getSmartName $  filter (isSignatory && hasSigned) (documentsignatorylinks document)
        F.value "someonesigned" $ not $ null $ filter (isSignatory && hasSigned) (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "link" $ protectLink forMail mctx $ LinkSignDoc (documentid document) signlink
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map authorattachmentname $ documentauthorattachments document -- TODO - check if this can be removed
        F.value "ispreview" $ not $ forMail
        F.value "previewLink" $  case (signatorylinkauthenticationtoviewmethod $ signlink) of
          StandardAuthenticationToView -> Just $ show $ LinkDocumentPreview (documentid document) (Just signlink <| forMail |> Nothing) (mainfile) 150
          _ -> Nothing
        F.value "hassigattachments" $ not $ null $ concat $ signatoryattachments <$> documentsignatorylinks document
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "nojavascriptmagic" $ True
        F.value "javascriptmagic" $ False
        F.value "companyname" $ emptyToNothing $ getCompanyName document


documentAttachedFields :: (MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m) => Bool -> SignatoryLink -> Bool -> Document -> Fields m ()
documentAttachedFields forMail signlink documentAttached document = do
  mctx <- getMailContext
  F.value "sign" $ signatoryispartner signlink
  F.value "documentAttached" documentAttached
  F.value "ispreview" $ not $ forMail
  F.value "mainfilelink" $ protectLink forMail mctx $ LinkMainFile document signlink
  mcompany <- case join $ maybesignatory <$> getAuthorSigLink document of
    Just suid ->  fmap Just $ (lift (dbQuery $ GetCompanyByUserID $ suid))
    Nothing -> return Nothing
  if ((companyallowsavesafetycopy . companyinfo) <$> mcompany) == Just True
     then do
       now <- currentTime
       F.value "availabledate" $ formatTimeYMD $ (unsavedDocumentLingerDays-1) `daysAfter` now
    else do
       now <- currentTime
       F.value "availabledate" $ formatTimeSimple $ (60 `minutesAfter` now)

remindMailSigned :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m)
                 => Bool
                 -> Maybe String
                 -> Document
                 -> SignatoryLink
                 -> Bool
                 -> m Mail
remindMailSigned forMail customMessage document signlink documentAttached = do
    documentMailWithDocLang document (templateName "remindMailSigned") $ do
            F.value "custommessage" $ asCustomMessage <$> customMessage
            documentAttachedFields forMail signlink documentAttached document

mailForwardSigned :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m)
                 => SignatoryLink -> Bool -> Document
                 -> m Mail
mailForwardSigned sl documentAttached document = do
  documentMailWithDocLang document (templateName "mailForwardSigned") $ do
    documentAttachedFields True sl documentAttached document


mailDocumentRejected :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
                     => Bool
                     -> Maybe String
                     -> Bool
                     -> SignatoryLink
                     -> Document
                     -> m Mail
mailDocumentRejected forMail customMessage forAuthor rejector document = do
   rejectorName <- smartOrUnnamedName rejector document
   documentMailWithDocLang document template $ do
        F.value "rejectorName" rejectorName
        F.value "ispreview" $ not $ forMail
        F.value "documentid" $ show $ documentid document
        F.value "signatorylinkid" $ show $ signatorylinkid rejector
        F.value "custommessage" $ asCustomMessage <$> customMessage
        F.value "hascustommessage" $ isJust customMessage
        F.value "companyname" $ emptyToNothing $ getCompanyName document
        F.value "loginlink" $ show $ LinkIssueDoc $ documentid document
  where template = if forAuthor then
                       templateName "mailAuthorRejectContractMail"
                   else
                       templateName "mailRejectContractMail"

smartOrUnnamedName :: TemplatesMonad m => SignatoryLink -> Document -> m String
smartOrUnnamedName sl doc | sn /= "" = return sn
                         | otherwise = do
                             prefix <- renderTemplate_ "_contractsignatoryname"
                             return $ prefix ++ " " ++ show signIndex
 where sn = getSmartName sl
       signatories = filter signatoryispartner $ documentsignatorylinks doc
       signIndex = case findIndex (\s -> signatorylinkid s == signatorylinkid sl) signatories of
                     Just i -> i + 1
                     Nothing -> 0

mailDocumentErrorForAuthor :: (HasLang a, MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) => a -> Document -> m Mail
mailDocumentErrorForAuthor authorlang document = do
   documentMail authorlang document (templateName "mailDocumentError") $ return ()

mailDocumentErrorForSignatory :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) => Document -> m Mail
mailDocumentErrorForSignatory document = do
   documentMailWithDocLang document (templateName "mailDocumentError") $ return ()


data InvitationTo = Sign | View
  deriving (Eq,Show)

fieldsInvitationTo :: TemplatesMonad m => InvitationTo -> Fields m ()
fieldsInvitationTo a = do
    F.value "sign" (a == Sign)
    F.value "view" (a == View)

mailInvitation :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
               => Bool
               -> InvitationTo
               -> Maybe SignatoryLink
               -> Document
               -> m Mail
mailInvitation forMail
               invitationto
               msiglink
               document = do
    mctx <- getMailContext
    let mainfile =  fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document) -- There always should be main file but tests fail without it
    documentMailWithDocLang document (templateName "mailInvitationToSignContract") $ do
        fieldsInvitationTo invitationto
        F.value "nojavascriptmagic" $ forMail
        F.value "javascriptmagic" $ not forMail
        F.value "hascustommessage" $ not $ null $ documentinvitetext document
        F.value "custommessage" $ asCustomMessage $ documentinvitetext document
        F.value "link" $ case msiglink of
          Just siglink -> Just $ makeFullLink mctx $ show (LinkSignDoc (documentid document) siglink)
          Nothing -> Nothing
        F.value "partners" $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $ map getSmartName $ filter (isSignatory && hasSigned) (documentsignatorylinks document)
        F.value "someonesigned" $ not $ null $ filter (isSignatory && hasSigned) (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "isattachments" $ length (documentauthorattachments document) > 0
        F.value "attachments" $ map authorattachmentname $ documentauthorattachments document -- TODO - check if this can be removed
        F.value "ispreview" $ not $ forMail
        F.value "previewLink" $  case (fromMaybe StandardAuthenticationToView $ signatorylinkauthenticationtoviewmethod <$> msiglink) of
          StandardAuthenticationToView -> Just $ show $ LinkDocumentPreview (documentid document) (msiglink <| forMail |> Nothing) (mainfile) 150
          _ -> Nothing
        F.value "hassigattachments" $ length (concatMap signatoryattachments $ documentsignatorylinks document ) > 0
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $ for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$> documentsignatorylinks document) $ \(link, sa) ->
                        (signatoryattachmentname sa, getSmartName link)
        F.value "companyname" $ emptyToNothing $ getCompanyName document


mailInvitationContent :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
                      => Bool
                      -> InvitationTo
                      -> Maybe SignatoryLink
                      -> Document
                      -> m String
mailInvitationContent  forMail invitationto msiglink document = do
     content <$> mailInvitation forMail invitationto msiglink document


mailClosedContent :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m)
                      => Bool
                      -> Document
                      -> m String
mailClosedContent ispreview document = do
     content <$> mailDocumentClosed ispreview (fromJust $ getAuthorSigLink document) False True document

mailDocumentClosed :: (MonadDB m, MonadThrow m, MonadTime m, TemplatesMonad m, MailContextMonad m) => Bool -> SignatoryLink -> Bool -> Bool -> Document -> m Mail
mailDocumentClosed ispreview sl sealFixed documentAttached document = do
   mctx <- getMailContext
   partylist <- renderLocalListTemplate document $ map getSmartName $ filter isSignatory (documentsignatorylinks document)
   let mainfile = fromMaybe (unsafeFileID 0) (mainfileid <$> documentsealedfile document `mplus` documentfile document) -- For preview we don't have a sealedd file yet
   documentMailWithDocLang document (templateName "mailContractClosed") $ do
        F.value "partylist" $ partylist
        F.value "signatoryname" $ getSmartName sl
        F.value "companyname" $ emptyToNothing $ getCompanyName document
        F.value "hasaccount" $ isJust $ maybesignatory sl
        F.value "doclink" $ if
          | ispreview -> Nothing
          | isAuthor sl -> Just $ makeFullLink mctx $ show (LinkIssueDoc $ documentid document)
          | otherwise -> Just $ makeFullLink mctx $ show (LinkSignDoc (documentid document) sl)
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (Nothing <| ispreview |> Just sl) (mainfile) 150
        F.value "sealFixed" $ sealFixed
        documentAttachedFields (not ispreview) sl documentAttached document
        F.value "closingtime" $ formatTime' "%Y-%m-%d %H:%M %Z" $ getLastSignedTime document
        F.value "custommessage" $ asCustomMessage <$> if (isAuthor sl && not ispreview)
                                    then Nothing
                                    else case (documentconfirmtext document) of
                                      "" -> Nothing
                                      s -> Just s



mailDocumentAwaitingForAuthor :: (HasLang a, MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) => a -> Document -> m Mail
mailDocumentAwaitingForAuthor authorlang document = do
    mctx <- getMailContext
    signatories <- renderLocalListTemplate authorlang $ map getSmartName $ filter (isSignatory && (not . isAuthor)) (documentsignatorylinks document)
    signatoriesThatSigned <- renderLocalListTemplate authorlang $ map getSmartName $ filter (isSignatory && hasSigned) (documentsignatorylinks document)
    let mainfile =  fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document) -- There always should be main file but tests fail without it
    documentMail authorlang document (templateName "mailDocumentAwaitingForAuthor") $ do
        F.value "authorname" $ getSmartName $ fromJust $ getAuthorSigLink document
        F.value "documentlink" $ makeFullLink mctx $ show $ LinkSignDoc (documentid document) $ fromJust $ getAuthorSigLink document
        F.value "partylist" signatories
        F.value "partylistSigned" signatoriesThatSigned
        F.value "someonesigned" $ not $ null $ filter (isSignatory && hasSigned) (documentsignatorylinks document)
        F.value "companyname" $ emptyToNothing $ getCompanyName document
        F.value "previewLink" $ show $ LinkDocumentPreview (documentid document) (getAuthorSigLink document) mainfile 150

-- helpers
makeFullLink :: MailContext -> String -> String
makeFullLink mctx link = mctxDomainUrl mctx ++ link

protectLink :: Bool -> MailContext -> KontraLink -> Maybe String
protectLink forMail mctx link
 | forMail   = Just $ makeFullLink mctx $ show link
 | otherwise = Nothing

documentMailWithDocLang :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) =>  Document -> String -> Fields m () -> m Mail
documentMailWithDocLang doc mailname otherfields = documentMail doc doc mailname otherfields

documentMailFields :: (MonadDB m, MonadThrow m, Monad m') => Document -> MailContext -> m (Fields m' ())
documentMailFields doc mctx = do
    mcompany <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                   Just suid ->  fmap Just $ dbQuery $ GetCompanyByUserID $ suid
                   Nothing -> return Nothing
    mcompanyui <- case mcompany of
                    Just comp -> (dbQuery $ GetCompanyUI (companyid comp)) >>= return . Just
                    Nothing -> return Nothing
    let themeid = fromMaybe (bdMailTheme $ mctxcurrentBrandedDomain mctx) (join $ companyMailTheme <$> mcompanyui)
    theme <- dbQuery $ GetTheme themeid
    return $ do
      F.value "ctxhostpart" $ mctxDomainUrl mctx
      F.value "ctxlang" (codeFromLang $ mctxlang mctx)
      F.value "documenttitle" $ documenttitle doc
      F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
      -- brandingdomainid and brandinguserid are needed only for preview/email logo
      F.value "brandingdomainid" (show . bdid . mctxcurrentBrandedDomain $ mctx)
      F.value "brandinguserid" (show <$> (join $ maybesignatory <$> getAuthorSigLink doc))
      brandingMailFields theme

documentMail :: (HasLang a, MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m) => a -> Document -> String -> Fields m () -> m Mail
documentMail haslang doc mailname otherfields = do
  mctx <- getMailContext
  mcompany <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
                   Just suid ->  fmap Just $ dbQuery $ GetCompanyByUserID $ suid
                   Nothing -> return Nothing
  mcompanyui <- case mcompany of
                    Just comp -> (dbQuery $ GetCompanyUI (companyid comp)) >>= return . Just
                    Nothing -> return Nothing
  let themeid = fromMaybe (bdMailTheme $ mctxcurrentBrandedDomain mctx) (join $ companyMailTheme <$> mcompanyui)
  theme <- dbQuery $ GetTheme themeid
  allfields <- documentMailFields doc mctx
  kontramaillocal (mctxcurrentBrandedDomain mctx) theme haslang mailname $ allfields >> otherfields

brandingMailFields :: Monad m => Theme -> Fields m ()
brandingMailFields theme = do
    -- MD5 is needed since some email client cache images based on cid
    F.value "logoAdler32" $ imageAdler32 $ themeLogo theme
    F.value "brandcolor" $ ensureHexRGB' $ themeBrandColor theme
    F.value "brandtextcolor" $ ensureHexRGB' $ themeBrandTextColor theme
    F.value "actioncolor" $ ensureHexRGB' $ themeActionColor theme
    F.value "actiontextcolor" $ ensureHexRGB' $ themeActionTextColor theme
    F.value "font"  $ themeFont theme
  where ensureHexRGB' s = fromMaybe s $ ensureHexRGB s

asCustomMessage :: String -> [String]
asCustomMessage = lines
