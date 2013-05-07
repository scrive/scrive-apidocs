{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument,
    draftIsChangingDocument
  ) where

import Control.Monad.Trans.Maybe
import Doc.SignatoryTMP
import Doc.DocStateData
import Utils.Monad
import Utils.Prelude
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

data DraftData = DraftData {
      title :: String
    , invitationmessage :: Maybe String
    , daystosign :: Int
    , authentication :: Maybe AuthenticationMethod
    , delivery :: Maybe DeliveryMethod
    , signatories :: [SignatoryTMP]
    , lang :: Maybe Lang
    , template :: Bool
    , tags :: Maybe [DocumentTag]
    , apicallbackurl :: Maybe String
    , process :: Maybe DocumentProcess
    } deriving Show

instance FromJSValue DocumentProcess where
  fromJSValue j = fromJSValue j >>= maybeRead

instance FromJSValue DocumentTag where
    fromJSValue = do
        name   <- fromJSValueField "name"
        value  <- fromJSValueField "value"
        case (name, value) of
             (Just n, Just v) -> return $ Just $ DocumentTag n v
             _ -> return Nothing

instance FromJSValue Lang where
    fromJSValue j =case fromJSValue j of -- Due to documentation inconsistency we need to support gb and en for a while.
      Just "se"    -> Just LANG_SV
      Just "sv"    -> Just LANG_SV
      Just "en"    -> Just LANG_EN
      Just "gb"    -> Just LANG_EN
      _            -> Nothing

instance FromJSValue DraftData where
   fromJSValue = do
        title' <- fromJSValueField "title"
        invitationmessage <-  fromJSValueField "invitationmessage"
        daystosign' <- fromJSValueField "daystosign"
        let minDaysToSign = 1
            maxDaysToSign = 90
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        signatories' <-  fromJSValueField "signatories"
        lang' <- fromJSValueField "lang"
        template' <- fromJSValueField "template"
        tags' <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValueM
        apicallbackurl' <- fromJSValueField "apicallbackurl"
        process' <- fromJSValueField "process"
        case (title', daystosign') of
            (Just t, Just daystosign)
             | daystosign >= minDaysToSign && daystosign <= maxDaysToSign ->
                return $ Just DraftData {
                                      title =  t
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authentication = authentication'
                                    , delivery = delivery'
                                    , signatories = concat $ maybeToList $ signatories'
                                    , lang = lang'
                                    , template = joinB template'
                                    , tags = tags'
                                    , apicallbackurl = apicallbackurl'
                                    , process = process'
                                 }
            _ -> return Nothing

applyDraftDataToDocument :: Kontrakcja m =>  Document -> DraftData -> Actor -> m (Either String Document)
applyDraftDataToDocument doc draft actor = do
    if (documentstatus doc /= Preparation)
     then do
       Log.error $ "Document is not in preparation, is in " ++ show (documentstatus doc)
       return $ Left $ "applyDraftDataToDocument failed"
     else do
      _ <- dbUpdate $ UpdateDraft (documentid doc) ( doc {
                                    documenttitle = title draft
                                  , documentinvitetext = fromMaybe (documentinvitetext doc) $ invitationmessage draft
                                  , documentdaystosign = daystosign draft
                                  , documentlang = fromMaybe (documentlang doc) (lang draft)
                                  , documenttags = fromMaybe (documenttags doc) (fmap Set.fromList $ tags draft)
                                  , documentapicallbackurl = (apicallbackurl draft)
                              }) actor
      when_ (template draft && (not $ isTemplate doc)) $ do
           dbUpdate $ TemplateFromDocument (documentid doc) actor
      Log.debug $ "Process: "  ++ show (process draft)
      when_ (isJust (process draft) && fromJust (process draft) /= toDocumentProcess (documenttype doc)) $ do
           Log.debug "Changing document process"
           dbUpdate $ SetDocumentProcess  (documentid doc) (fromJust (process draft)) actor
      case (mergeSignatories draft (fromJust $ getAuthorSigLink doc) (signatories draft)) of
           Nothing   -> return $ Left "Problem with author details while sending draft"
           Just sigs -> do
             mdoc <- runMaybeT $ do
               True <- dbUpdate $ ResetSignatoryDetails2 (documentid doc) sigs actor
               Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
               return newdoc
             return $ case mdoc of
               Nothing  -> Left "applyDraftDataToDocument failed"
               Just newdoc -> Right newdoc

mergeSignatories :: DraftData
                 -> SignatoryLink
                 -> [SignatoryTMP]
                 -> Maybe [(SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload, Maybe String, AuthenticationMethod, DeliveryMethod)]
mergeSignatories docdraft docAuthor tmps =
        let
            (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstantDetails =  setFstname (getFirstName docAuthor) .
                                        setSndname (getLastName docAuthor) .
                                        setEmail   (getEmail docAuthor)
            mapAuthenticationMethod (a,b,c,d,e,f) =
              case authentication docdraft of
                Just x -> (a,b,c,d,x,f)
                _ -> (a,b,c,d,e,f)
            mapDeliveryMethod (a,b,c,d,e,f) =
              case delivery docdraft of
                Just x -> (a,b,c,d,e,x)
                _ -> (a,b,c,d,e,f)

        in case (atmp) of
                ([authorTMP]) -> Just $ map (mapDeliveryMethod . 
                                             mapAuthenticationMethod . 
                                             toSignatoryDetails2) $
                                              (setAuthorConstantDetails authorTMP) : notatmps
                _ -> Nothing


draftIsChangingDocument :: DraftData -> Document -> Bool
draftIsChangingDocument DraftData{..} doc@Document{..}  =
        (title /= documenttitle)
     || ((isJust invitationmessage) && (fromJust invitationmessage) /= documentinvitetext)
     || ((isJust lang) && (fromJust lang) /= documentlang)
     || ((isJust process) && (fromJust process) /= toDocumentProcess documenttype)
     || ((isJust tags) && (Set.fromList $ fromJust tags) /= documenttags)
     || (apicallbackurl /= documentapicallbackurl)
     || (template /= isTemplate doc)
     || (daystosign /= documentdaystosign)
     || (isJust authentication && all ((/=) (fromJust authentication)) (fmap signatorylinkauthenticationmethod documentsignatorylinks))
     || (isJust delivery && all ((/=) (fromJust delivery)) (fmap signatorylinkdeliverymethod documentsignatorylinks))
     || (draftIsChangingDocumentSignatories signatories documentsignatorylinks)

draftIsChangingDocumentSignatories :: [SignatoryTMP] -> [SignatoryLink] -> Bool
draftIsChangingDocumentSignatories (stmp:stmps) (sl:sls) = (signatoryTMPIsChangingSignatoryLink stmp sl) || (draftIsChangingDocumentSignatories stmps sls)
draftIsChangingDocumentSignatories [] [] = False
draftIsChangingDocumentSignatories _ _ = True
