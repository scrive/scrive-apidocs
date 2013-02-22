{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument
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
    , delivery :: DeliveryMethod
    , signatories :: [SignatoryTMP]
    , lang :: Maybe Lang
    , template :: Bool
    , tags :: Maybe [DocumentTag]
    , apicallbackurl :: Maybe String
    , process :: Maybe DocumentProcess
    } deriving Show

instance FromJSValue AuthenticationMethod where
  fromJSValue j = case fromJSValue j of
    Just "standard" -> Just StandardAuthentication
    Just "eleg"     -> Just ELegAuthentication
    _               -> Nothing

instance FromJSValue DeliveryMethod where
  fromJSValue j = case fromJSValue j of
    Just "email" -> Just EmailDelivery
    Just "pad"   -> Just PadDelivery
    Just "api"   -> Just APIDelivery
    _            -> Nothing

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
        delivery' <-  fromJSValueField "delivery"
        signatories' <-  fromJSValueField "signatories"
        lang' <- fromJSValueField "lang"
        template' <- fromJSValueField "template"
        tags' <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValueM
        apicallbackurl' <- fromJSValueField "apicallbackurl"
        process' <- fromJSValueField "process"
        case (title', daystosign', delivery') of
            (Just t, Just daystosign, Just d)
             | daystosign >= minDaysToSign && daystosign <= maxDaysToSign ->
                return $ Just DraftData {
                                      title =  t
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , delivery = d
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
                                  , documentdeliverymethod = delivery draft
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
      case (mergeSignatories (fromJust $ getAuthorSigLink doc) (signatories draft)) of
           Nothing   -> return $ Left "Problem with author details while sending draft"
           Just sigs -> do
             mdoc <- runMaybeT $ do
               True <- dbUpdate $ ResetSignatoryDetails2 (documentid doc) sigs actor
               Just newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
               return newdoc
             return $ case mdoc of
               Nothing  -> Left "applyDraftDataToDocument failed"
               Just newdoc -> Right newdoc

mergeSignatories :: SignatoryLink -> [SignatoryTMP] -> Maybe [(SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload, Maybe String)]
mergeSignatories docAuthor tmps =
        let (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstandDetails =  setFstname (getFirstName docAuthor) .
                                        setSndname (getLastName docAuthor) .
                                        setEmail   (getEmail docAuthor)
        in case (atmp) of
                ([authorTMP]) -> Just $ map toSignatoryDetails2 $ (setAuthorConstandDetails authorTMP) : notatmps
                _ -> Nothing
