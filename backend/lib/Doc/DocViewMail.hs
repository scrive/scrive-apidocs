module Doc.DocViewMail
    ( mailDocumentAwaitingForAuthor
    , mailDocumentClosed
    , mailDocumentErrorForAuthor
    , mailDocumentErrorForSignatory
    , mailDocumentRejected
    , mailForwardSigningForAuthor
    , mailForwardSigningForNewSignatory
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
import Crypto.RNG
import Data.Label.Base
import Data.Time (UTCTime(..))
import Text.StringTemplates.Templates
import qualified Data.Label.Partial as LP
import qualified Text.StringTemplates.Fields as F

import BrandedDomain.BrandedDomain
import Branding.Adler32
import DB
import Doc.DocInfo (getLastSignedOrApprovedTime)
import Doc.DocStateData
import Doc.DocUtils
import Doc.Model.Update
import File.FileID
import KontraLink
import MagicHash (MagicHash)
import MailContext
import Mails.SendMail
import MinutesTime
import Templates
import Theme.Model
import User.Model
import UserGroup.Model
import UserGroup.Types
import Util.HasSomeUserInfo
import Util.SignatoryLinkUtils
import Utils.Color
import Utils.Monoid

mailDocumentRemind :: ( CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m
                      , TemplatesMonad m, MailContextMonad m )
                   => Bool -> Maybe String -> Document -> SignatoryLink -> Bool
                   -> m Mail
mailDocumentRemind automatic customMessage doc sigLink documentAttached =
  case ( documentstatus doc, maybesigninfo sigLink
       , isSignatory sigLink || isApprover sigLink ) of
    (_, Nothing, True)  ->
      remindMailNotSigned automatic True customMessage doc sigLink
    (Pending, _, False) ->
      remindMailNotSigned automatic True customMessage doc sigLink
    _                   ->
      remindMailSigned    True customMessage doc sigLink documentAttached

mailDocumentRemindContent :: ( CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m
                             , TemplatesMonad m, MailContextMonad m )
                          => Maybe String -> Document -> SignatoryLink -> Bool
                          -> m String
mailDocumentRemindContent customMessage doc sigLink documentAttached = do
  content <$> case ( documentstatus doc, maybesigninfo sigLink
                   , isSignatory sigLink || isApprover sigLink ) of
    (_, Nothing, True)  ->
      remindMailNotSigned False False customMessage doc sigLink
    (Pending, _, False) ->
      remindMailNotSigned False False customMessage doc sigLink
    _                   ->
      remindMailSigned    False customMessage doc sigLink documentAttached

remindMailNotSigned :: ( CryptoRNG m, MonadDB m, MonadThrow m, TemplatesMonad m
                       , MailContextMonad m )
                    => Bool -> Bool -> Maybe String -> Document -> SignatoryLink
                    -> m Mail
remindMailNotSigned automatic forMail customMessage document signlink = do
    mctx <- getMailContext
    let mainfile   = fromMaybe (unsafeFileID 0)
                     (mainfileid <$> documentfile document)
        authorname = getAuthorName document
    documentMailWithDocLang document
      (templateName "remindMailNotSignedContract") $ do
        F.value "sign" $ isSignatory signlink
        F.value "approve" $ isApprover signlink
        F.value "custommessage" $ asCustomMessage <$> customMessage
        F.value "authorname" authorname
        F.value "automatic" automatic
        F.value "partners" $
          map getSmartName $
          filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $
          map getSmartName $
          filter isSignatoryAndHasSigned (documentsignatorylinks document)
        F.value "someonesigned" $
          not $ null $
          filter isSignatoryAndHasSigned (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "link" $
          protectLink forMail mctx $
          LinkSignDoc (documentid document) signlink
        F.value "isattachments" $
          length (documentauthorattachments document) > 0
        F.value "attachments" $
          map authorattachmentname $
          documentauthorattachments document -- TODO - check if this
                                             -- can be removed
        F.value "ispreview" $ not $ forMail
        F.value "previewLink" $
          case (signatorylinkauthenticationtoviewmethod $ signlink) of
            StandardAuthenticationToView ->
              Just $ show $
              LinkDocumentPreview (documentid document)
              (Just signlink <| forMail |> Nothing) (mainfile) 150
            _ -> Nothing
        F.value "hassigattachments" $
          not $ null $ concat $ signatoryattachments <$>
          documentsignatorylinks document
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $
          for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$>
               documentsignatorylinks document) $
          \(link, sa) ->
            (signatoryattachmentname sa, getSmartName link)
        F.value "nojavascriptmagic" $ True
        F.value "javascriptmagic" $ False
        F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document


documentAttachableFields :: ( CryptoRNG m, MailContextMonad m, MonadDB m
                            , MonadThrow m, MonadTime m )
                         => Bool -> SignatoryLink -> Bool
                         -> Maybe (MagicHash, UTCTime)
                            -- ^ If we need a link, give a magic hash
                         -> Document -> Fields m ()
documentAttachableFields forMail signlink forceLink mmh document = do
  mctx <- getMailContext
  F.value "sign"      $ isSignatory signlink
  F.value "approve"   $ isApprover signlink
  F.value "forceLink" $ forceLink
  F.value "ispreview" $ not $ forMail
  case mmh of
    Nothing -> return ()
    Just (mh, expiration) -> do
      F.value "mainfilelink" $ protectLink forMail mctx $
        LinkMainFile document signlink mh
      F.value "availabledate" $ formatTimeYMD expiration

remindMailSigned :: ( CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m
                    , TemplatesMonad m, MailContextMonad m )
                 => Bool
                 -> Maybe String
                 -> Document
                 -> SignatoryLink
                 -> Bool
                 -> m Mail
remindMailSigned forMail customMessage document signlink documentAttached = do
  mhtime <- makeTemporaryMagicHash signlink documentAttached False
  documentMailWithDocLang document (templateName "remindMailSigned") $ do
    F.value "custommessage" $ asCustomMessage <$> customMessage
    documentAttachableFields forMail signlink False mhtime document

mailForwardSigned :: ( CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m
                     , TemplatesMonad m, MailContextMonad m )
                  => SignatoryLink -> Bool -> Document
                  -> m Mail
mailForwardSigned sl documentAttached document = do
  mmh <- makeTemporaryMagicHash sl documentAttached False
  documentMailWithDocLang document (templateName "mailForwardSigned") $ do
    documentAttachableFields True sl False mmh document

mailDocumentRejected :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                        , MailContextMonad m )
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
        F.value "rejectorIsApprover" $ isApprover rejector
        F.value "ispreview" $ not $ forMail
        F.value "documentid" $ show $ documentid document
        F.value "signatorylinkid" $ show $ signatorylinkid rejector
        F.value "custommessage" $ asCustomMessage <$> customMessage
        F.value "hascustommessage" $ isJust customMessage
        F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
        F.value "loginlink" $ show $ LinkIssueDoc $ documentid document
  where template = if forAuthor then
                       templateName "mailAuthorRejectContractMail"
                   else
                       templateName "mailRejectContractMail"

mailForwardSigningForAuthor :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                        , MailContextMonad m )
                     => SignatoryLink
                     -> SignatoryLink
                     -> Document
                     -> m Mail
mailForwardSigningForAuthor originalsl newsl doc = do
   fromName <- smartOrUnnamedName originalsl doc
   toName <- smartOrUnnamedName newsl doc
   documentMailWithDocLang doc "mailForwardSigningForAuthorMail" $ do
        F.value "fromName" fromName
        F.value "toName" $ toName
        F.value "loginlink" $ show $ LinkIssueDoc $ documentid doc

mailForwardSigningForNewSignatory :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                        , MailContextMonad m )
                     => Maybe String
                     -> SignatoryLink
                     -> SignatoryLink
                     -> Document
                     -> m Mail
mailForwardSigningForNewSignatory message originalsl newsl doc = do
   mctx <- getMailContext
   fromName <- smartOrUnnamedName originalsl doc
   toName <- smartOrUnnamedName newsl doc
   documentMailWithDocLang doc "mailForwardSigningForNewSignatory" $ do
        F.value "hasforwardmessage" $ isJust message
        F.value "forwardmessage" $ asCustomMessage <$> message
        F.value "hasinvitationmessage" $ (documentinvitetext doc) /= ""
        F.value "invitationmessage" $ asCustomMessage (documentinvitetext doc)
        F.value "fromName" $ fromName
        F.value "toName" $ toName
        F.value "authorname" $ getSmartName <$> getAuthorSigLink doc
        F.value "link" $ makeFullLink mctx $ show $ (LinkSignDoc (documentid doc) newsl)

smartOrUnnamedName :: TemplatesMonad m => SignatoryLink -> Document -> m String
smartOrUnnamedName sl doc
  | sn /= ""  = return sn
  | otherwise = do
      prefix <- renderTemplate_ "_contractsignatoryname"
      return $ prefix ++ " " ++ show signIndex
        where
          sn          = getSmartName sl
          signatories = filter (isSignatory || isApprover) $ documentsignatorylinks doc
          signIndex   =
            case findIndex
                 (\s -> signatorylinkid s == signatorylinkid sl) signatories
            of Just i  -> i + 1
               Nothing -> 0

mailDocumentErrorForAuthor :: ( HasLang a, MonadDB m, MonadThrow m
                              , TemplatesMonad m, MailContextMonad m)
                           => a -> Document -> m Mail
mailDocumentErrorForAuthor authorlang document = do
  documentMail authorlang document (templateName "mailDocumentError") $
    return ()

mailDocumentErrorForSignatory :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                                 , MailContextMonad m )
                              => Document -> m Mail
mailDocumentErrorForSignatory document = do
  documentMailWithDocLang document (templateName "mailDocumentError") $
    return ()


data InvitationTo = Sign | Approve | View
  deriving (Eq,Show)

fieldsInvitationTo :: TemplatesMonad m => InvitationTo -> Fields m ()
fieldsInvitationTo a = do
    F.value "sign"    (a == Sign)
    F.value "view"    (a == View)
    F.value "approve" (a == Approve)

mailInvitation :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                  , MailContextMonad m )
               => Bool -> InvitationTo -> Maybe SignatoryLink -> Document
               -> m Mail
mailInvitation forMail
               invitationto
               msiglink
               document = do
    mctx <- getMailContext
    let mainfile = fromMaybe (unsafeFileID 0)
                   -- There always should be main file but tests fail
                   -- without it.
                   (mainfileid <$> documentfile document)
    documentMailWithDocLang document
      (templateName "mailInvitationToSignContract") $ do
        fieldsInvitationTo invitationto
        F.value "nojavascriptmagic" $ forMail
        F.value "javascriptmagic" $ not forMail
        F.value "hascustommessage" $ not $ null $ documentinvitetext document
        F.value "custommessage" $ asCustomMessage $ documentinvitetext document
        F.value "link" $ case msiglink of
          Just siglink -> Just $ makeFullLink mctx $
                          show (LinkSignDoc (documentid document) siglink)
          Nothing      -> Nothing
        F.value "partners" $ map getSmartName $
          filter isSignatory (documentsignatorylinks document)
        F.value "partnerswhosigned" $
          map getSmartName $
          filter isSignatoryAndHasSigned (documentsignatorylinks document)
        F.value "someonesigned" $
          not $ null $
          filter isSignatoryAndHasSigned (documentsignatorylinks document)
        F.value "timetosign" $ show <$> documenttimeouttime document
        F.value "isattachments" $
          length (documentauthorattachments document) > 0
        F.value "attachments" $ map authorattachmentname $
          documentauthorattachments document -- TODO - check if this
                                             -- can be removed
        F.value "ispreview" $ not $ forMail
        F.value "previewLink" $
          case (fromMaybe StandardAuthenticationToView $
                signatorylinkauthenticationtoviewmethod <$> msiglink)
          of
            StandardAuthenticationToView ->
              Just $ show $
              LinkDocumentPreview (documentid document)
              (msiglink <| forMail |> Nothing) (mainfile) 150
            _ -> Nothing
        F.value "hassigattachments" $
          length (concatMap signatoryattachments $
                  documentsignatorylinks document)
          > 0
        -- We try to use generic templates and this is why we return a tuple
        F.value "sigattachments" $
          for (concat $ (\l -> (\a -> (l,a)) <$> signatoryattachments l) <$>
               documentsignatorylinks document) $
          \(link, sa) ->
            (signatoryattachmentname sa, getSmartName link)
        F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document

mailInvitationContent :: ( MonadDB m, MonadThrow m, TemplatesMonad m
                         , MailContextMonad m )
                      => Bool -> InvitationTo -> Maybe SignatoryLink -> Document
                      -> m String
mailInvitationContent forMail invitationto msiglink document = do
  content <$> mailInvitation forMail invitationto msiglink document

mailClosedContent :: ( CryptoRNG m, MonadDB m, MonadThrow m, MonadTime m
                     , TemplatesMonad m, MailContextMonad m )
                  => Bool -> Document -> m String
mailClosedContent ispreview document =
  content <$> mailDocumentClosed ispreview
    (fromJust $ getAuthorSigLink document) False True False document

mailDocumentClosed :: ( CryptoRNG m, MailContextMonad m, MonadDB m, MonadThrow m, MonadTime m
                      , TemplatesMonad m )
                   => Bool -> SignatoryLink -> Bool
                   -> Bool -- ^ Would the attachments fit inside an email?
                   -> Bool -- ^ Force link usage.
                   -> Document -> m Mail
mailDocumentClosed ispreview sl sealFixed documentAttachable forceLink document = do
   mctx <- getMailContext
   mmh <- makeTemporaryMagicHash sl documentAttachable forceLink
   partylist <- renderLocalListTemplate document $
                map getSmartName $
                filter isSignatory (documentsignatorylinks document)
   let mainfile = fromMaybe (unsafeFileID 0)
                  (mainfileid <$> documentsealedfile document
                    -- For preview we don't have a sealed file yet
                    `mplus` documentfile document)
   documentMailWithDocLang document (templateName "mailContractClosed") $ do
        F.value "partylist" $ partylist
        F.value "signatoryname" $ getSmartName sl
        F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
        F.value "hasaccount" $ isJust $ maybesignatory sl
        F.value "doclink" $ if
          | ispreview -> Nothing
          | isAuthor sl -> Just $ makeFullLink mctx $
                           show (LinkIssueDoc $ documentid document)
          | otherwise -> Just $ makeFullLink mctx $
                         show (LinkSignDoc (documentid document) sl)
        F.value "previewLink" $ show $
          LinkDocumentPreview (documentid document)
          (Nothing <| ispreview |> Just sl) (mainfile) 150
        F.value "sealFixed" $ sealFixed
        documentAttachableFields (not ispreview) sl forceLink mmh document
        F.value "closingtime" $
          formatTime' "%Y-%m-%d %H:%M %Z" $
          getLastSignedOrApprovedTime document
        F.value "custommessage" $
          asCustomMessage <$>
          if (isAuthor sl && not ispreview)
          then Nothing
          else case (documentconfirmtext document) of
                 "" -> Nothing
                 s  -> Just s

mailDocumentAwaitingForAuthor :: ( HasLang a, MonadDB m, MonadThrow m
                                 , TemplatesMonad m, MailContextMonad m )
                              => a -> Document -> m Mail
mailDocumentAwaitingForAuthor authorlang document = do
    mctx <- getMailContext
    signatories <- renderLocalListTemplate authorlang $
                   map getSmartName $
                   filter (isSignatory && (not . isAuthor))
                   (documentsignatorylinks document)
    signatoriesThatSigned <-
      renderLocalListTemplate authorlang $
      map getSmartName $
      filter isSignatoryAndHasSigned (documentsignatorylinks document)
    -- There always should be main file but tests fail without it
    let mainfile = fromMaybe (unsafeFileID 0)
                   (mainfileid <$> documentfile document)
    documentMail authorlang document
      (templateName "mailDocumentAwaitingForAuthor") $ do
        F.value "authorname" $ getSmartName $
          fromJust $ getAuthorSigLink document
        F.value "documentlink" $ makeFullLink mctx $
          show $ LinkSignDoc (documentid document) $
          fromJust $ getAuthorSigLink document
        F.value "partylist" signatories
        F.value "partylistSigned" signatoriesThatSigned
        F.value "someonesigned" $
          not $ null $
          filter isSignatoryAndHasSigned (documentsignatorylinks document)
        F.value "companyname" $ emptyToNothing $ getAuthorCompanyName document
        F.value "previewLink" $ show $
          LinkDocumentPreview (documentid document)
          (getAuthorSigLink document) mainfile 150

-- Helpers.

makeFullLink :: MailContext -> String -> String
makeFullLink mctx link = get mctxDomainUrl mctx ++ link

protectLink :: Bool -> MailContext -> KontraLink -> Maybe String
protectLink forMail mctx link
 | forMail   = Just $ makeFullLink mctx $ show link
 | otherwise = Nothing

documentMailWithDocLang :: ( MonadDB m, MonadThrow m
                           , TemplatesMonad m, MailContextMonad m )
                        =>  Document -> String -> Fields m () -> m Mail
documentMailWithDocLang doc mailname otherfields =
  documentMail doc doc mailname otherfields

documentMailFields :: (MonadDB m, MonadThrow m, Monad m')
                   => Document -> MailContext -> m (Fields m' ())
documentMailFields doc mctx = do
    mug <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
             Just suid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ suid
             Nothing   -> return Nothing
    let themeid = fromMaybe (get (bdMailTheme . mctxcurrentBrandedDomain) mctx)
                  . LP.get (just . uguiMailTheme . ugUI . just) $ mug
    theme <- dbQuery $ GetTheme themeid
    return $ do
      F.value "ctxhostpart" $ get mctxDomainUrl mctx
      F.value "ctxlang" (codeFromLang $ get mctxlang mctx)
      F.value "documenttitle" $ documenttitle doc
      F.value "creatorname" $ getSmartName $ fromJust $ getAuthorSigLink doc
      -- brandingdomainid and brandinguserid are needed only for
      -- preview/email logo
      F.value "brandingdomainid"
        (show $ get (bdid . mctxcurrentBrandedDomain) mctx)
      F.value "brandinguserid"
        (show <$> (join $ maybesignatory <$> getAuthorSigLink doc))
      brandingMailFields theme

documentMail :: ( HasLang a, MailContextMonad m, MonadDB m
                , MonadThrow m, TemplatesMonad m )
             => a -> Document -> String -> Fields m () -> m Mail
documentMail haslang doc mailname otherfields = do
  mctx <- getMailContext
  mug <- case (join $ maybesignatory <$> getAuthorSigLink doc) of
           Just suid -> fmap Just $ dbQuery $ UserGroupGetByUserID $ suid
           Nothing   -> return Nothing
  let themeid = fromMaybe (get (bdMailTheme . mctxcurrentBrandedDomain) mctx)
                . LP.get (just . uguiMailTheme . ugUI . just) $ mug
  theme <- dbQuery $ GetTheme themeid
  allfields <- documentMailFields doc mctx
  kontramaillocal (get mctxmailNoreplyAddress mctx)
    (get mctxcurrentBrandedDomain mctx) theme haslang mailname $ allfields
    >> otherfields

brandingMailFields :: Monad m => Theme -> Fields m ()
brandingMailFields theme = do
    -- MD5 is needed since some email client cache images based on cid
    F.value "logoAdler32"     $ imageAdler32 $ themeLogo theme
    F.value "brandcolor"      $ ensureHexRGB' $ themeBrandColor theme
    F.value "brandtextcolor"  $ ensureHexRGB' $ themeBrandTextColor theme
    F.value "actioncolor"     $ ensureHexRGB' $ themeActionColor theme
    F.value "actiontextcolor" $ ensureHexRGB' $ themeActionTextColor theme
    F.value "font"            $ themeFont theme
  where ensureHexRGB' s = fromMaybe s $ ensureHexRGB s

asCustomMessage :: String -> [String]
asCustomMessage = lines

-- | Create a temporary hash valid for 30 days if we need to send a link.
-- Otherwise, return Nothing.
makeTemporaryMagicHash
  :: (CryptoRNG m, MonadDB m, MonadTime m) => SignatoryLink -> Bool -> Bool
  -> m (Maybe (MagicHash, UTCTime))
makeTemporaryMagicHash sl documentAttachable forceLink = do
  if documentAttachable && not forceLink
    then return Nothing
    else do
      now <- currentTime
      -- Make it valid until the end of the 30th day.
      let expiration = (30 `daysAfter` now) { utctDayTime = 86399 }
      mh <- dbUpdate $ NewTemporaryMagicHash (signatorylinkid sl) expiration
      return $ Just (mh, expiration)
