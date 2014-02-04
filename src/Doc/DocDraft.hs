{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    applyDraftDataToDocument,
    draftIsChangingDocument
  ) where

import API.Monad (serverError)
import Control.Exception.Lifted (throwIO)
import Doc.DocInfo (isPreparation)
import Doc.DocStateData
import Utils.Monad
import Data.Maybe
import Kontra
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Data.List
import User.Lang
import Doc.Model
import DB
import Util.Actor
import Text.JSON.FromJSValue
import qualified Data.Set as Set
import Utils.Read
import qualified Log
import Control.Conditional (whenM, unlessM)
import Control.Monad
import InputValidation
import Data.Functor
import Utils.Default
import Doc.DocumentMonad (DocumentMonad, theDocument)
import Doc.DocUtils
import Data.String.Utils (strip)
import Doc.SignatoryLinkID
-- JSON instances

instance MatchWithJSValue SignatoryLink where
    matchesWithJSValue s = do
      mid    <- fromJSValueField "id"
      return (Just (signatorylinkid s) == (maybeRead =<< mid))

instance FromJSValueWithUpdate SignatoryLink where
    fromJSValueWithUpdate ms = do
        author <- fromJSValueField "author"
        signs  <- fromJSValueField "signs"
        mfields <- fromJSValueFieldCustom "fields" (fromJSValueManyWithUpdate $ fromMaybe [] (signatoryfields <$> ms))
        signorder <- fromJSValueField "signorder"
        attachments <- fromJSValueField "attachments"
        (csv :: Maybe (Maybe CSVUpload)) <- fromJSValueField "csv"
        (sredirecturl :: Maybe (Maybe String)) <- fromJSValueField "signsuccessredirect"
        (rredirecturl :: Maybe (Maybe String)) <- fromJSValueField "rejectredirect"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        confirmationdelivery' <-  fromJSValueField "confirmationdelivery"
        case (mfields) of
             (Just fields) -> return $ Just $ defaultValue {
                    signatorylinkid            = fromMaybe (unsafeSignatoryLinkID 0) (signatorylinkid <$> ms)
                  , signatorysignorder     = updateWithDefaultAndField (SignOrder 1) signatorysignorder (SignOrder <$> signorder)
                  , signatoryfields        = fields
                  , signatoryisauthor      = updateWithDefaultAndField False signatoryisauthor author
                  , signatoryispartner     = updateWithDefaultAndField False signatoryispartner signs
                  , signatorylinkcsvupload       = updateWithDefaultAndField Nothing signatorylinkcsvupload csv
                  , signatoryattachments         = updateWithDefaultAndField [] signatoryattachments attachments
                  , signatorylinksignredirecturl = updateWithDefaultAndField Nothing signatorylinksignredirecturl sredirecturl
                  , signatorylinkrejectredirecturl = updateWithDefaultAndField Nothing signatorylinkrejectredirecturl rredirecturl
                  , signatorylinkauthenticationmethod = updateWithDefaultAndField StandardAuthentication signatorylinkauthenticationmethod authentication'
                  , signatorylinkdeliverymethod       = updateWithDefaultAndField EmailDelivery signatorylinkdeliverymethod delivery'
                  , signatorylinkconfirmationdeliverymethod = updateWithDefaultAndField EmailConfirmationDelivery signatorylinkconfirmationdeliverymethod confirmationdelivery'
                }
             _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryLink -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf ms))





instance FromJSValue SignatoryField where
    fromJSValue = fromJSValueWithUpdate Nothing


instance FromJSValueWithUpdate SignatoryField where
    fromJSValueWithUpdate msf =  do
        ftype <- fromJSValue
        value  <- fromJSValueField "value"
        obligatory <- updateWithDefaultAndField True sfObligatory <$> fromJSValueField "obligatory"
        filledbysender <- updateWithDefaultAndField False sfObligatory <$> fromJSValueField "shouldbefilledbysender"
        placements <- updateWithDefaultAndField [] sfPlacements <$> fromJSValueField "placements"
        case (ftype,value) of
          (Just ft, Just v) -> do
              let v' = case ft of
                        EmailFT -> strip v
                        SignatureFT _ -> ""
                        _ -> v
              return $ Just $ SignatoryField ft v' obligatory filledbysender placements
          _ -> return Nothing
      where
       updateWithDefaultAndField :: a -> (SignatoryField -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf msf))


instance MatchWithJSValue SignatoryField where
    matchesWithJSValue sf = do
      ftype <- fromJSValue
      return (ftype ==  Just(sfType sf))

instance FromJSValue SignatoryAttachment where
    fromJSValue = do
        name<- fromJSValueField "name"
        description  <- fromJSValueField "description"
        case (name,description) of
             (Just n, Just d) -> return $ Just $ SignatoryAttachment {signatoryattachmentname  = n ,
                                                                      signatoryattachmentdescription = d,
                                                                      signatoryattachmentfile = Nothing}
             _ -> return Nothing

instance FromJSValue FieldType where
   fromJSValue = do
    s <- fromJSValueField "name"
    t <- fromJSValueField "type"
    filled <- (not . null) <$> fromMaybe ("" :: String) <$> fromJSValueField "value"
    return $ case (fromMaybe "standard" t,s) of
         ("standard",  Just "fstname")    -> Just $ FirstNameFT
         ("standard",  Just "sndname")    -> Just $ LastNameFT
         ("standard",  Just "email")      -> Just $ EmailFT
         ("standard",  Just "mobile")     -> Just $ MobileFT
         ("standard",  Just "sigpersnr")  -> Just $ PersonalNumberFT
         ("standard",  Just "sigco")      -> Just $ CompanyFT
         ("standard",  Just "sigcompnr")  -> Just $ CompanyNumberFT
         ("signature", Just name       )  -> Just $ SignatureFT name
         ("custom",    Just name       )  -> Just $ CustomFT name filled
         ("checkbox",  Just name       )  -> Just $ CheckboxFT name
         _ -> Nothing


instance FromJSValue CSVUpload  where
    fromJSValue = do
        rows <- fromJSValue
        case rows of
             Just rs -> return $ Just $ CSVUpload
                        { csvtitle = ""
                        , csvcontents = rs
                        }
             _ -> return Nothing

instance FromJSValue DocumentTag where
    fromJSValue = do
        name   <- fromJSValueField "name"
        value  <- fromJSValueField "value"
        case (name, value) of
             (Just n, Just v) -> return $ Just $ DocumentTag n v
             _ -> return Nothing

instance FromJSValue Lang where
    fromJSValue = do
      j <- fromJSValue
      return $ case j of -- Due to documentation inconsistency we need to support gb and en for a while.
        Just "se"    -> Just LANG_SV
        Just "sv"    -> Just LANG_SV
        Just "en"    -> Just LANG_EN
        Just "gb"    -> Just LANG_EN
        Just "de"    -> Just LANG_DE
        _            -> Nothing


instance FromJSValueWithUpdate Document where
    fromJSValueWithUpdate mdoc = do
        title <- fromJSValueField "title"
        (invitationmessage :: Maybe (Maybe String)) <-  fromJSValueField "invitationmessage"
        (confirmationmessage :: Maybe (Maybe String)) <-  fromJSValueField "confirmationmessage"
        daystosign <- fromJSValueField "daystosign"
        daystoremind <- fromJSValueField "daystoremind"
        showheader <- fromJSValueField "showheader"
        showpdfdownload <- fromJSValueField "showpdfdownload"
        showrejectoption <- fromJSValueField "showrejectoption"
        showfooter <- fromJSValueField "showfooter"
        authentication <-  fromJSValueField "authentication"
        delivery <-  fromJSValueField "delivery"
        signatories <-  fromJSValueFieldCustom "signatories" (fromJSValueManyWithUpdate (fromMaybe [] $ documentsignatorylinks <$> mdoc))
        lang <- fromJSValueField "lang"
        doctype <- fmap (\t -> if t then Template else Signable) <$> fromJSValueField "template"
        tags <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValue
        (apicallbackurl :: Maybe (Maybe String)) <- fromJSValueField "apicallbackurl"
        authorattachments <- fromJSValueFieldCustom "authorattachments" $ fromJSValueCustomMany $ fmap (join . (fmap maybeRead)) $ (fromJSValueField "id")
        let daystosign'  = min 90 $ max 1 $ updateWithDefaultAndField 14 documentdaystosign daystosign
        let daystoremind' = min daystosign' <$> max 1 <$> updateWithDefaultAndField Nothing documentdaystoremind daystoremind

        return $ Just defaultValue {
            documenttitle = updateWithDefaultAndField "" documenttitle title,
            documentlang  = updateWithDefaultAndField LANG_SV documentlang lang,
            documentinvitetext = case (invitationmessage) of
                                     Nothing -> fromMaybe "" $ documentinvitetext <$> mdoc
                                     Just Nothing -> ""
                                     Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s),
            documentconfirmtext = case (confirmationmessage) of
                                     Nothing -> fromMaybe "" $ documentconfirmtext <$> mdoc
                                     Just Nothing -> ""
                                     Just (Just s) -> fromMaybe "" (resultToMaybe $ asValidInviteText s),
            documentdaystosign   = daystosign',
            documentdaystoremind = daystoremind',
            documentshowheader = updateWithDefaultAndField True documentshowheader showheader,
            documentshowpdfdownload = updateWithDefaultAndField True documentshowpdfdownload showpdfdownload,
            documentshowrejectoption = updateWithDefaultAndField True documentshowrejectoption showrejectoption,
            documentshowfooter = updateWithDefaultAndField True documentshowfooter showfooter,
            documentsignatorylinks = mapAuth authentication $ mapDL delivery $ updateWithDefaultAndField [] documentsignatorylinks signatories,
            documentauthorattachments = updateWithDefaultAndField [] documentauthorattachments (fmap AuthorAttachment <$> authorattachments),
            documenttags = updateWithDefaultAndField Set.empty documenttags (Set.fromList <$> tags),
            documenttype = updateWithDefaultAndField Signable documenttype doctype,
            documentapicallbackurl = updateWithDefaultAndField Nothing documentapicallbackurl apicallbackurl
          }
      where
       updateWithDefaultAndField :: a -> (Document -> a) -> Maybe a -> a
       updateWithDefaultAndField df uf mv = fromMaybe df (mv `mplus` (fmap uf mdoc))
       mapDL :: Maybe DeliveryMethod -> [SignatoryLink] -> [SignatoryLink]
       mapDL Nothing sls = sls
       mapDL (Just dl) sls = map (\sl -> sl {signatorylinkdeliverymethod = dl}) sls
       mapAuth :: Maybe AuthenticationMethod -> [SignatoryLink] -> [SignatoryLink]
       mapAuth Nothing sls = sls
       mapAuth (Just au) sls = map (\sl -> sl {signatorylinkauthenticationmethod = au}) sls


applyDraftDataToDocument :: (Kontrakcja m, DocumentMonad m) =>  Document -> Actor -> m ()
applyDraftDataToDocument draft actor = do
    unlessM (isPreparation <$> theDocument) $ do
      theDocument >>= \doc -> Log.attention_ $ "Document is not in preparation, is in " ++ show (documentstatus doc)
      throwIO $ SomeKontraException $ serverError "applyDraftDataToDocument failed"
    _ <- theDocument >>= \doc -> dbUpdate $ UpdateDraft doc{
                                  documenttitle = documenttitle draft
                                , documentinvitetext = documentinvitetext draft
                                , documentconfirmtext = documentconfirmtext draft
                                , documentdaystosign = documentdaystosign draft
                                , documentdaystoremind = documentdaystoremind draft
                                , documentshowheader = documentshowheader draft
                                , documentshowpdfdownload = documentshowpdfdownload draft
                                , documentshowrejectoption = documentshowrejectoption draft
                                , documentshowfooter = documentshowfooter draft
                                , documentlang = documentlang draft
                                , documenttags = documenttags draft
                                , documentapicallbackurl = documentapicallbackurl draft
                                } actor
    whenM ((\doc -> isTemplate draft && (not $ isTemplate doc)) <$> theDocument) $ do
         dbUpdate $ TemplateFromDocument actor
    documentauthorattachments <$> theDocument >>= \atts -> forM_ atts $ \att -> do
            when_ (not $ att `elem` (documentauthorattachments draft)) $ do
              dbUpdate $ RemoveDocumentAttachment (authorattachmentfile att) actor
    documentsignatorylinks <$> theDocument >>= \siglinks -> case (mergeAuthorDetails siglinks (sortBy compareSL $ documentsignatorylinks draft)) of
         Nothing   -> throwIO $ SomeKontraException $ serverError "Problem with author details while sending draft"
         Just sigs -> do
           -- Checking if some integrations depend on fact that we don't change fstname and lastname for author. To be removed till 20. II.
           let  (Just oldAuthor) = find isAuthor $ documentsignatorylinks $ draft
           let  (Just newAuthor) = find isAuthor sigs
           when (getFirstName oldAuthor /= getFirstName newAuthor || getLastName oldAuthor /= getLastName newAuthor) $ do
            Log.mixlog_ $ "Checkup: Update could changed author details from " ++ getFullName oldAuthor ++ " to " ++ getFullName newAuthor
           -- End testing

           res <- dbUpdate $ ResetSignatoryDetails (sortBy compareSL $ sigs) actor
           unless res $ throwIO $ SomeKontraException $ serverError "applyDraftDataToDocument failed"


compareSL :: SignatoryLink -> SignatoryLink -> Ordering
compareSL s1 s2 | signatoryisauthor s1 = LT
                | signatoryisauthor s2 = GT
                | signatorylinkid s2 == unsafeSignatoryLinkID 0 = LT
                | signatorylinkid s1 == unsafeSignatoryLinkID 0 = GT
                | otherwise = compare (signatorylinkid s1) (signatorylinkid s2)




mergeAuthorDetails :: [SignatoryLink] ->[SignatoryLink] -> Maybe [SignatoryLink]
mergeAuthorDetails sigs nsigs =
          let
            (nasig', nsigs') = partition isAuthor nsigs
            (asig, _) = partition isAuthor sigs
            setConstantDetails a =  replaceFieldValue FirstNameFT (getFirstName a) .
                                    replaceFieldValue LastNameFT (getLastName a) .
                                    replaceFieldValue EmailFT   (getEmail a) .
                                    (\s -> s {maybesignatory = maybesignatory a}) .  -- We need to be sure that we will not disconnect author
                                    (\s -> s {signatorylinkid = signatorylinkid a})  -- And we try to keep original id of author signatory
          in case (asig, nasig') of
               ([asig'], [nasig'']) -> Just $ (setConstantDetails asig' nasig'') : nsigs'
               _ -> Nothing


-- TODO can't we refactor this to take a list of documenttitle, documentinvitetext etc?
draftIsChangingDocument :: Document -> Document -> Bool
draftIsChangingDocument draft doc =
        (documenttitle draft /= documenttitle doc)
     || (documentinvitetext draft /= documentinvitetext doc)
     || (documentlang draft /= documentlang doc)
     || (documenttags draft /= documenttags doc)
     || (documentapicallbackurl draft /= documentapicallbackurl doc)
     || (isTemplate draft /= isTemplate doc)
     || (documentdaystosign draft /= documentdaystosign doc)
     || (documentdaystoremind draft /= documentdaystoremind doc)
     || (documentshowheader draft /= documentshowheader doc)
     || (documentshowpdfdownload draft /= documentshowpdfdownload doc)
     || (documentshowrejectoption draft /= documentshowrejectoption doc)
     || (documentshowfooter draft /= documentshowfooter doc)
     || (draftIsChangingDocumentSignatories (documentsignatorylinks draft) (documentsignatorylinks doc))

draftIsChangingDocumentSignatories :: [SignatoryLink] -> [SignatoryLink] -> Bool
draftIsChangingDocumentSignatories (sl':sls') (sl:sls) = (newSignatorySignatoryLinkIsChangingSignatoryLink sl' sl) || (draftIsChangingDocumentSignatories sls' sls)
draftIsChangingDocumentSignatories [] [] = False
draftIsChangingDocumentSignatories _ _ = True


newSignatorySignatoryLinkIsChangingSignatoryLink :: SignatoryLink -> SignatoryLink -> Bool
newSignatorySignatoryLinkIsChangingSignatoryLink newsl sl =
        (signatorylinkid newsl /= signatorylinkid sl)
     || (signatoryfields newsl /= signatoryfields sl)
     || (signatoryisauthor newsl /= signatoryisauthor sl)
     || (signatoryispartner newsl /= signatoryispartner sl)
     || (signatorysignorder newsl /= signatorysignorder sl)
     || (signatoryattachments newsl /= signatoryattachments sl)
     || (signatorylinkcsvupload newsl /= signatorylinkcsvupload sl)
     || (signatorylinksignredirecturl newsl /= signatorylinksignredirecturl sl)
     || (signatorylinkauthenticationmethod newsl /= signatorylinkauthenticationmethod sl)
     || (signatorylinkdeliverymethod newsl /= signatorylinkdeliverymethod sl)
