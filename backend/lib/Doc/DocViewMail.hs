module Doc.DocViewMail
    ( mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentErrorForAuthor
    , mailDocumentErrorForSignatory
    , mailDocumentRejected
    , mailDocumentTimedout
    , mailForwardSigningForAuthor
    , mailForwardSigningForNewSignatory
    , mailDocumentRemind
    , mailDocumentRemindContent
    , mailPartyProcessFinalizedNotification
    , mailForwardSigned
    , mailPortalInviteWithUser
    , mailPortalInviteWithoutUser
    , mailPortalRemindWithUser
    , mailPortalRemindWithoutUser
    , InvitationTo(..)
    , mailInvitation
    , mailInvitationContent
    , mailClosedContent
    , documentMailWithDocLang
    , brandingMailFields
    ) where

import Control.Conditional ((<|), (|>))
import Control.Monad.Catch
import Crypto.RNG
import Data.Time (UTCTime(..))
import Optics (_Just)
import Text.StringTemplates.Templates
import qualified Data.Text as T
import qualified Text.StringTemplates.Fields as F

import Branding.Adler32
import DB
import Doc.DocInfo (getLastSignedOrApprovedTime, isClosed)
import Doc.DocStateData
import Doc.DocUtils
import Doc.Model.Update
import Doc.Types.SignatoryAccessToken
import Doc.Types.SignatoryLink (ProcessFinishedAction(..))
import File.FileID
import KontraLink
import MagicHash (MagicHash)
import MailContext
import Mails.KontraInfoForMail (KontraInfoForMail(..))
import Mails.SendMail
import MinutesTime
import Templates
import Theme.Model
import User.Email
import User.Model
import User.UserAccountRequest
import UserGroup.Model
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Color
import Utils.Monoid

mailDocumentRemind
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => Bool
  -> Maybe Text
  -> Document
  -> SignatoryLink
  -> Bool
  -> m Mail
mailDocumentRemind automatic customMessage doc sigLink documentAttached =
  case
      ( documentstatus doc
      , maybesigninfo sigLink
      , isSignatory sigLink || isApprover sigLink
      )
    of
      (_, Nothing, True) -> remindMailNotSigned automatic True customMessage doc sigLink
      (Pending, _, False) -> remindMailNotSigned automatic True customMessage doc sigLink
      _ -> remindMailSigned True customMessage doc sigLink documentAttached

mailDocumentRemindContent
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => Maybe Text
  -> Document
  -> SignatoryLink
  -> Bool
  -> m Text
mailDocumentRemindContent customMessage doc sigLink documentAttached = do
  content
    <$> case
          ( documentstatus doc
          , maybesigninfo sigLink
          , isSignatory sigLink || isApprover sigLink
          )
        of
          (_, Nothing, True) -> remindMailNotSigned False False customMessage doc sigLink
          (Pending, _, False) ->
            remindMailNotSigned False False customMessage doc sigLink
          _ -> remindMailSigned False customMessage doc sigLink documentAttached

remindMailNotSigned
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => Bool
  -> Bool
  -> Maybe Text
  -> Document
  -> SignatoryLink
  -> m Mail
remindMailNotSigned automatic forMail customMessage document signlink = do
  mctx <- getMailContext
  let mainfile   = fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
      authorname = getAuthorName document
  mh <- makeInvitationMagicHash signlink
  documentMailWithDocLang document (templateName "remindMailNotSignedContract") $ do
    F.value "sign" $ isSignatory signlink
    F.value "approve" $ isApprover signlink
    F.value "custommessage" $ asCustomMessage <$> customMessage
    F.value "authorname" authorname
    F.value "automatic" automatic
    F.value "partners" $ map getSmartName $ filter isSignatory
                                                   (documentsignatorylinks document)
    F.value "partnerswhosigned" $ map getSmartName $ filter
      isSignatoryAndHasSigned
      (documentsignatorylinks document)
    F.value "someonesigned" $ not $ null $ filter isSignatoryAndHasSigned
                                                  (documentsignatorylinks document)
    F.value "timetosign" $ show <$> documenttimeouttime document
    F.value "link" $ protectLink forMail mctx $ LinkSignDocMagicHash
      (documentid document)
      (signatorylinkid signlink)
      mh
    F.value "isattachments" $ length (documentauthorattachments document) > 0
    F.value "attachments" $ map authorattachmentname $ documentauthorattachments document -- TODO - check if this
                                         -- can be removed
    F.value "ispreview" $ not $ forMail
    F.value "previewLink" $ case (signatorylinkauthenticationtoviewmethod $ signlink) of
      StandardAuthenticationToView -> Just $ show $ LinkDocumentPreview
        (documentid document)
        (Just (signlink, Just mh) <| forMail |> Nothing)
        (mainfile)
        150
      _ -> Nothing
    F.value "hassigattachments"
      $   not
      $   null
      $   concat
      $   signatoryattachments
      <$> documentsignatorylinks document
    -- We try to use generic templates and this is why we return a tuple
    F.value "sigattachments"
      $ for
          (   concat
          $   (\l -> (\a -> (l, a)) <$> signatoryattachments l)
          <$> documentsignatorylinks document
          )
      $ \(link, sa) -> (signatoryattachmentname sa, getSmartName link)
    F.value "nojavascriptmagic" $ True
    F.value "javascriptmagic" $ False
    F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document


documentAttachableFields
  :: (CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m)
  => Bool
  -> SignatoryLink
  -> Bool
  -> Maybe (MagicHash, UTCTime)
                            -- ^ If we need a link, give a magic hash
  -> Document
  -> Fields m ()
documentAttachableFields forMail signlink forceLink mhtime document = do
  mctx <- getMailContext
  F.value "sign" $ isSignatory signlink
  F.value "approve" $ isApprover signlink
  F.value "forceLink" $ forceLink
  F.value "ispreview" $ not $ forMail
  case mhtime of
    Nothing               -> return ()
    Just (mh, expiration) -> do
      F.value "mainfilelink" $ protectLink forMail mctx $ LinkMainFile document
                                                                       signlink
                                                                       mh
      F.value "availabledate" $ formatTimeYMD expiration
      F.value "doclink" $ protectLink forMail mctx $ if isAuthor signlink
        then LinkIssueDoc $ documentid document
        else LinkSignDocMagicHash (documentid document) (signatorylinkid signlink) mh

remindMailSigned
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => Bool
  -> Maybe Text
  -> Document
  -> SignatoryLink
  -> Bool
  -> m Mail
remindMailSigned forMail mcustomMessage document signlink documentAttached = do
  mhtime <- if documentAttached
    then return Nothing
    else Just <$> makeConfirmationMagicHash signlink
  documentMailWithDocLang document (templateName "remindMailSigned") $ do
    F.value "custommessage" $ asCustomMessage $ fromMaybe "" mcustomMessage
    F.value "hascustommessage" $ case mcustomMessage of
      Nothing -> False
      Just "" -> False
      Just _  -> True
    F.value "shouldhavefilelink" $ isJust mhtime -- for previews
    documentAttachableFields forMail signlink False mhtime document

mailForwardSigned
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => SignatoryLink
  -> Bool
  -> Document
  -> m Mail
mailForwardSigned sl documentAttached document = do
  mhtime <- if documentAttached
    then return Nothing
    else Just <$> makeConfirmationMagicHash sl
  documentMailWithDocLang document (templateName "mailForwardSigned") $ do
    documentAttachableFields True sl False mhtime document

mailDocumentRejected
  :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
  => Bool
  -> Maybe Text
  -> Bool
  -> SignatoryLink
  -> Document
  -> m Mail
mailDocumentRejected forMail customMessage forAuthor rejector document = do
  rejectorName <- smartOrUnnamedName rejector document
  documentMailWithDocLang document template $ do
    F.value "rejectorName" rejectorName
    F.value "rejectorIsApprover" $ isApprover rejector
    F.value "ispreview" $ not $ forMail
    F.value "documentid" $ show $ documentid document
    F.value "signatorylinkid" $ show $ signatorylinkid rejector
    F.value "custommessage" $ asCustomMessage <$> customMessage
    F.value "hascustommessage" $ isJust customMessage
    F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
    F.value "loginlink" $ show $ LinkIssueDoc $ documentid document
  where
    template = if forAuthor
      then templateName "mailAuthorRejectContractMail"
      else templateName "mailRejectContractMail"

mailDocumentTimedout
  :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) => Document -> m Mail
mailDocumentTimedout document = do
  documentMailWithDocLang document (templateName "mailTimedoutMail") $ do
    F.value "documentlink" $ show $ LinkIssueDoc $ documentid document

mailForwardSigningForAuthor
  :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
  => SignatoryLink
  -> SignatoryLink
  -> Document
  -> m Mail
mailForwardSigningForAuthor originalsl newsl doc = do
  fromName <- smartOrUnnamedName originalsl doc
  toName   <- smartOrUnnamedName newsl doc
  documentMailWithDocLang doc "mailForwardSigningForAuthorMail" $ do
    F.value "fromName" fromName
    F.value "toName" $ toName
    F.value "signing" $ signatoryrole newsl == SignatoryRoleSigningParty
    F.value "loginlink" $ show $ LinkIssueDoc $ documentid doc

mailForwardSigningForNewSignatory
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => Maybe Text
  -> SignatoryLink
  -> SignatoryLink
  -> Document
  -> m Mail
mailForwardSigningForNewSignatory message originalsl newsl doc = do
  mctx     <- getMailContext
  fromName <- smartOrUnnamedName originalsl doc
  toName   <- smartOrUnnamedName newsl doc
  mh       <- makeInvitationMagicHash newsl
  documentMailWithDocLang doc "mailForwardSigningForNewSignatory" $ do
    F.value "hasforwardmessage" $ isJust message
    F.value "forwardmessage" $ asCustomMessage <$> message
    F.value "hasinvitationmessage" $ (documentinvitetext doc) /= ""
    F.value "invitationmessage" $ asCustomMessage (documentinvitetext doc)
    F.value "fromName" $ fromName
    F.value "toName" $ toName
    F.value "authorname" $ getSmartName <$> getAuthorSigLink doc
    F.value "signing" $ signatoryrole newsl == SignatoryRoleSigningParty
    F.value "link" $ makeFullLink mctx $ showt $ LinkSignDocMagicHash
      (documentid doc)
      (signatorylinkid newsl)
      mh

smartOrUnnamedName :: TemplatesMonad m => SignatoryLink -> Document -> m Text
smartOrUnnamedName sl doc
  | sn /= "" = return sn
  | otherwise = do
    prefix <- renderTextTemplate_ "_contractsignatoryname"
    return $ prefix <> " " <> (showt signIndex)
  where
    sn          = getSmartName sl
    signatories = filter (isSignatory || isApprover) $ documentsignatorylinks doc
    signIndex =
      case findIndex (\s -> signatorylinkid s == signatorylinkid sl) signatories of
        Just i  -> i + 1
        Nothing -> 0

mailPortalInviteWithUser
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => User
  -> Text
  -> Email
  -> Text
  -> m Mail
mailPortalInviteWithUser authorUser portalUrl email _name = do
  otherMail (Just authorUser) (templateName "mailPortalInviteWithUser") $ do
    F.value "link" $ show (LinkPortalInviteWithAccount portalUrl email)
    F.value "authorname" $ getSmartName authorUser

mailPortalInviteWithoutUser
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => User
  -> Text
  -> Email
  -> Text
  -> UserAccountRequest
  -> m Mail
mailPortalInviteWithoutUser authorUser portalUrl email _name uar = do
  otherMail (Just authorUser) (templateName "mailPortalInviteWithoutUser") $ do
    F.value "link" $ show
      (LinkPortalInviteWithoutAccount portalUrl email (uarToken uar) (uarExpires uar))
    F.value "authorname" $ getSmartName authorUser

mailPortalRemindWithUser
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => User
  -> Text
  -> Email
  -> Text
  -> m Mail
mailPortalRemindWithUser authorUser portalUrl email _name = do
  otherMail (Just authorUser) (templateName "mailPortalRemindWithUser") $ do
    F.value "link" $ show (LinkPortalInviteWithAccount portalUrl email)
    F.value "authorname" $ getSmartName authorUser

mailPortalRemindWithoutUser
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => User
  -> Text
  -> Email
  -> Text
  -> UserAccountRequest
  -> m Mail
mailPortalRemindWithoutUser authorUser portalUrl email _name uar = do
  otherMail (Just authorUser) (templateName "mailPortalRemindWithoutUser") $ do
    F.value "link" $ show
      (LinkPortalInviteWithoutAccount portalUrl email (uarToken uar) (uarExpires uar))
    F.value "authorname" $ getSmartName authorUser

mailDocumentErrorForAuthor
  :: (HasLang a, MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
  => a
  -> Document
  -> m Mail
mailDocumentErrorForAuthor authorlang document = do
  documentMail authorlang document (templateName "mailDocumentError") $ return ()

mailDocumentErrorForSignatory
  :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m) => Document -> m Mail
mailDocumentErrorForSignatory document = do
  documentMailWithDocLang document (templateName "mailDocumentError") $ return ()

mailPartyProcessFinalizedNotification
  :: ( CryptoRNG m
     , MonadDB m
     , MonadTime m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => Document
  -> SignatoryLink
  -> ProcessFinishedAction
  -> m Mail
mailPartyProcessFinalizedNotification document signatoryLink action = do
  mailCtx <- getMailContext
  let documentId = (documentid document)
      template   = case action of
        DocumentSigned | hasConfirmationDelivery signatoryLink ->
          templateName "mailDocumentSignedNotificationWithConfirmation"
        DocumentSigned -> templateName "mailDocumentSignedNotification"
        DocumentApproved | hasConfirmationDelivery signatoryLink ->
          templateName "mailDocumentApprovedNotificationWithConfirmation"
        DocumentApproved -> templateName "mailDocumentApprovedNotification"
      mainfile = fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
  (mh, mtime) <- if isClosed document
    then do
      (mh, time) <- makeConfirmationMagicHash signatoryLink
      return (mh, Just time)
    else (, Nothing) <$> makeInvitationMagicHash signatoryLink
  email <- documentMailWithDocLang document (templateName template) $ do
    fieldsInvitationTo View
    F.value "link" $ makeFullLink mailCtx $ showt $ LinkSignDocMagicHash
      documentId
      (signatorylinkid signatoryLink)
      mh
    F.value "availabledate" $ fmap formatTimeYMD mtime
    F.value "previewLink" $ show $ LinkDocumentPreview (documentid document)
                                                       (Just (signatoryLink, Just mh))
                                                       mainfile
                                                       150
  return email { to                = [getMailAddress signatoryLink]
               , kontraInfoForMail = Just $ OtherDocumentMail documentId
               }


data InvitationTo = Sign | Approve | View
  deriving (Eq,Show)

fieldsInvitationTo :: TemplatesMonad m => InvitationTo -> Fields m ()
fieldsInvitationTo a = do
  F.value "sign" (a == Sign)
  F.value "view" (a == View)
  F.value "approve" (a == Approve)

mailInvitation
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => Bool
  -> InvitationTo
  -> Maybe SignatoryLink
  -> Document
  -> m Mail
mailInvitation forMail invitationto msiglink document = do
  mctx <- getMailContext
  let mainfile = fromMaybe (unsafeFileID 0)
                 -- There always should be main file but tests fail
                 -- without it.
                           (mainfileid <$> documentfile document)
  msiglinkmh <- case msiglink of
    Nothing      -> return Nothing
    Just siglink -> do
      mh <- makeInvitationMagicHash siglink
      return $ Just (siglink, Just mh)
  documentMailWithDocLang document (templateName "mailInvitationToSignContract") $ do
    fieldsInvitationTo invitationto
    F.value "nojavascriptmagic" $ forMail
    F.value "javascriptmagic" $ not forMail
    F.value "hascustommessage" $ not $ T.null $ documentinvitetext document
    F.value "custommessage" $ asCustomMessage $ documentinvitetext document
    F.value "link" $ case msiglinkmh of
      Just (siglink, Just mh) -> Just $ makeFullLink mctx $ showt $ LinkSignDocMagicHash
        (documentid document)
        (signatorylinkid siglink)
        mh
      _ -> Nothing
    F.value "partners" $ map getSmartName $ filter isSignatory
                                                   (documentsignatorylinks document)
    F.value "partnerswhosigned" $ map getSmartName $ filter
      isSignatoryAndHasSigned
      (documentsignatorylinks document)
    F.value "someonesigned" $ not $ null $ filter isSignatoryAndHasSigned
                                                  (documentsignatorylinks document)
    F.value "timetosign" $ show <$> documenttimeouttime document
    F.value "isattachments" $ length (documentauthorattachments document) > 0
    F.value "attachments" $ map authorattachmentname $ documentauthorattachments document -- TODO - check if this
                                         -- can be removed
    F.value "ispreview" $ not $ forMail
    F.value "previewLink"
      $ case
          (   fromMaybe StandardAuthenticationToView
          $   signatorylinkauthenticationtoviewmethod
          <$> msiglink
          )
        of
          StandardAuthenticationToView -> Just $ show $ LinkDocumentPreview
            (documentid document)
            (msiglinkmh <| forMail |> Nothing)
            (mainfile)
            150
          _ -> Nothing
    F.value "hassigattachments"
      $ length (concatMap signatoryattachments $ documentsignatorylinks document)
      > 0
    -- We try to use generic templates and this is why we return a tuple
    F.value "sigattachments"
      $ for
          (   concat
          $   (\l -> (\a -> (l, a)) <$> signatoryattachments l)
          <$> documentsignatorylinks document
          )
      $ \(link, sa) -> (signatoryattachmentname sa, getSmartName link)
    F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document

mailInvitationContent
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , TemplatesMonad m
     , MailContextMonad m
     , MonadTime m
     )
  => Bool
  -> InvitationTo
  -> Maybe SignatoryLink
  -> Document
  -> m Text
mailInvitationContent forMail invitationto msiglink document = do
  content <$> mailInvitation forMail invitationto msiglink document

mailClosedContent
  :: ( CryptoRNG m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => Bool
  -> Document
  -> m Text
mailClosedContent ispreview document =
  content
    <$> mailDocumentClosed ispreview
                           (fromJust $ getAuthorSigLink document)
                           False
                           True
                           False
                           document

mailDocumentClosed
  :: ( CryptoRNG m
     , MailContextMonad m
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     )
  => Bool
  -> SignatoryLink
  -> Bool
  -> Bool -- ^ Would the attachments fit inside an email?
  -> Bool -- ^ Force link usage.
  -> Document
  -> m Mail
mailDocumentClosed ispreview sl sealFixed documentAttachable forceLink document = do
  mhtime <- if documentAttachable && not ispreview && not forceLink
    then return Nothing
    else Just <$> makeConfirmationMagicHash sl
  partylist <- renderLocalListTemplate document $ map getSmartName $ filter
    isSignatory
    (documentsignatorylinks document)
  let mainfile = fromMaybe
        (unsafeFileID 0)
        (       mainfileid
        <$>     documentsealedfile document
                   -- For preview we don't have a sealed file yet
        `mplus` documentfile document
        )
  documentMailWithDocLang document (templateName "mailContractClosed") $ do
    F.value "partylist" $ partylist
    F.value "signatoryname" $ getSmartName sl
    F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
    F.value "hasaccount" $ isJust $ maybesignatory sl
    F.value "previewLink" $ showt $ LinkDocumentPreview
      (documentid document)
      (Nothing <| ispreview |> Just (sl, fst <$> mhtime))
      (mainfile)
      150
    F.value "sealFixed" $ sealFixed
    documentAttachableFields (not ispreview) sl forceLink mhtime document
    F.value "closingtime" $ formatTime' "%Y-%m-%d %H:%M %Z" $ getLastSignedOrApprovedTime
      document
    F.value "custommessage" $ asCustomMessage <$> if (isAuthor sl && not ispreview)
      then Nothing
      else case (documentconfirmtext document) of
        "" -> Nothing
        s  -> Just s

mailDocumentAwaitingForAuthor
  :: ( CryptoRNG m
     , HasLang a
     , MonadDB m
     , MonadThrow m
     , MonadTime m
     , TemplatesMonad m
     , MailContextMonad m
     )
  => a
  -> Document
  -> m Mail
mailDocumentAwaitingForAuthor authorlang document = do
  mctx        <- getMailContext
  signatories <- renderLocalListTemplate authorlang $ map getSmartName $ filter
    (isSignatory && (not . isAuthor))
    (documentsignatorylinks document)
  signatoriesThatSigned <- renderLocalListTemplate authorlang $ map getSmartName $ filter
    isSignatoryAndHasSigned
    (documentsignatorylinks document)
  -- There always should be main file but tests fail without it
  let mainfile = fromMaybe (unsafeFileID 0) (mainfileid <$> documentfile document)
      author   = fromJust $ getAuthorSigLink document
  mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid author)
                                           SignatoryAccessTokenForMailBeforeClosing
                                           Nothing
  documentMail authorlang document (templateName "mailDocumentAwaitingForAuthor") $ do
    F.value "authorname" $ getSmartName author
    F.value "documentlink" $ makeFullLink mctx $ showt $ LinkSignDocMagicHash
      (documentid document)
      (signatorylinkid author)
      mh
    F.value "partylist" signatories
    F.value "partylistSigned" signatoriesThatSigned
    F.value "someonesigned" $ not $ null $ filter isSignatoryAndHasSigned
                                                  (documentsignatorylinks document)
    F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
    F.value "previewLink" $ show $ LinkDocumentPreview (documentid document)
                                                       (Just (author, Just mh))
                                                       mainfile
                                                       150

-- Helpers.

makeFullLink :: MailContext -> Text -> Text
makeFullLink mctx link = mctx ^. mctxDomainUrl <> link

protectLink :: Bool -> MailContext -> KontraLink -> Maybe Text
protectLink forMail mctx link | forMail   = Just $ makeFullLink mctx $ showt link
                              | otherwise = Nothing

documentMailWithDocLang
  :: (MonadDB m, MonadThrow m, TemplatesMonad m, MailContextMonad m)
  => Document
  -> Text
  -> Fields m ()
  -> m Mail
documentMailWithDocLang doc mailname otherfields =
  documentMail doc doc mailname otherfields

documentMailFields
  :: (MonadDB m, MonadThrow m, Monad m') => Document -> MailContext -> m (Fields m' ())
documentMailFields doc mctx = do
  mug <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
    Just suid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ suid
    Nothing   -> return Nothing
  let themeid = fromMaybe (mctx ^. #brandedDomain % #mailTheme)
        $ preview (_Just % #ui % #uguiMailTheme % _Just) mug
  theme <- dbQuery $ GetTheme themeid
  return $ do
    F.value "ctxhostpart" $ mctx ^. mctxDomainUrl
    F.value "ctxlang" (codeFromLang $ mctx ^. #lang)
    F.value "documenttitle" $ documenttitle doc
    F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
    -- brandingdomainid and brandinguserid are needed only for
    -- preview/email logo
    F.value "brandingdomainid" (show $ mctx ^. #brandedDomain % #id)
    F.value "brandinguserid" (show <$> (maybesignatory =<< getAuthorSigLink doc))
    brandingMailFields theme

otherMailFields
  :: (MonadDB m, MonadThrow m, Monad m') => Maybe User -> MailContext -> m (Fields m' ())
otherMailFields muser mctx = do
  mug <- case (userid <$> muser) of
    Just uid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ uid
    Nothing  -> return Nothing
  let themeid = fromMaybe (mctx ^. #brandedDomain % #mailTheme)
        $ preview (_Just % #ui % #uguiMailTheme % _Just) mug
  theme <- dbQuery $ GetTheme themeid
  return $ do
    F.value "ctxhostpart" $ mctx ^. mctxDomainUrl
    F.value "ctxlang" (codeFromLang $ mctx ^. #lang)
    -- brandingdomainid and brandinguserid are needed only for
    -- preview/email logo
    F.value "brandingdomainid" (show $ mctx ^. #brandedDomain % #id)
    F.value "brandinguserid" (show <$> userid <$> muser)
    brandingMailFields theme

documentMail
  :: (HasLang a, MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m)
  => a
  -> Document
  -> Text
  -> Fields m ()
  -> m Mail
documentMail haslang doc mailname otherfields = do
  mctx <- getMailContext
  mug  <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
    Just suid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ suid
    Nothing   -> return Nothing
  let themeid = fromMaybe (mctx ^. #brandedDomain % #mailTheme)
        $ preview (_Just % #ui % #uguiMailTheme % _Just) mug
  theme     <- dbQuery $ GetTheme themeid
  allfields <- documentMailFields doc mctx
  kontramaillocal (mctx ^. #mailNoreplyAddress)
                  (mctx ^. #brandedDomain)
                  theme
                  haslang
                  mailname
    $  allfields
    >> otherfields

otherMail
  :: (MailContextMonad m, MonadDB m, MonadThrow m, TemplatesMonad m)
  => Maybe User
  -> Text
  -> Fields m ()
  -> m Mail
otherMail muser mailname otherfields = do
  let lang = fromMaybe defaultLang $ getLang <$> muser
  mctx <- getMailContext
  mug  <- case (userid <$> muser) of
    Just uid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ uid
    Nothing  -> return Nothing
  let themeid = fromMaybe (mctx ^. #brandedDomain % #mailTheme)
        $ preview (_Just % #ui % #uguiMailTheme % _Just) mug
  theme     <- dbQuery $ GetTheme themeid
  allfields <- otherMailFields muser mctx
  kontramaillocal (mctx ^. #mailNoreplyAddress)
                  (mctx ^. #brandedDomain)
                  theme
                  lang
                  mailname
    $  allfields
    >> otherfields

brandingMailFields :: Monad m => Theme -> Fields m ()
brandingMailFields theme = do
    -- MD5 is needed since some email client cache images based on cid
  F.value "logoAdler32" $ imageAdler32 $ themeLogo theme
  F.value "brandcolor" $ ensureHexRGB' $ themeBrandColor theme
  F.value "brandtextcolor" $ ensureHexRGB' $ themeBrandTextColor theme
  F.value "actioncolor" $ ensureHexRGB' $ themeActionColor theme
  F.value "actiontextcolor" $ ensureHexRGB' $ themeActionTextColor theme
  F.value "font" $ themeFont theme
  where ensureHexRGB' s = fromMaybe s $ ensureHexRGB $ T.unpack s

asCustomMessage :: Text -> [Text]
asCustomMessage = T.lines

-- | Create a temporary hash valid until the document gets closed.
makeInvitationMagicHash
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m) => SignatoryLink -> m MagicHash
makeInvitationMagicHash sl = dbUpdate $ NewSignatoryAccessToken
  (signatorylinkid sl)
  SignatoryAccessTokenForMailBeforeClosing
  Nothing

-- | Create a temporary hash valid for 30 days.
makeConfirmationMagicHash
  :: (CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m)
  => SignatoryLink
  -> m (MagicHash, UTCTime)
makeConfirmationMagicHash sl = do
  --if documentAttachable && not forceLink
  now <- currentTime
  -- Make it valid until the end of the 30th day.
  let expiration = (30 `daysAfter` now) { utctDayTime = 86399 }
  mh <- dbUpdate $ NewSignatoryAccessToken (signatorylinkid sl)
                                           SignatoryAccessTokenForMailAfterClosing
                                           (Just expiration)
  return (mh, expiration)
