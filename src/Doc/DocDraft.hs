{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument
  ) where

import Control.Monad.Trans.Maybe
import Doc.SignatoryTMP
import Doc.DocStateData
import Utils.Monad
import Utils.Monoid
import Utils.Prelude
import Control.Monad
import Data.Maybe
import Kontra
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Data.List
import User.Region
import Doc.Model
import DB
import Util.Actor
import Text.JSON.FromJSValue
import qualified Data.Set as Set

data DraftData = DraftData {
      title :: String
    , invitationmessage :: Maybe String
    , daystosign :: Maybe Int
    , authentication :: AuthenticationMethod
    , delivery :: DeliveryMethod
    , signatories :: [SignatoryTMP]
    , region :: Maybe Region
    , template :: Bool
    , tags :: Maybe [DocumentTag]
    , apicallbackurl :: Maybe String
    } deriving Show

instance FromJSValue AuthenticationMethod where
  fromJSValue j = case fromJSValue j of
    Just "email" -> Just EmailAuthentication
    Just "eleg"  -> Just ELegAuthentication
    _            -> Nothing

instance FromJSValue DeliveryMethod where
  fromJSValue j = case fromJSValue j of
    Just "email" -> Just EmailDelivery
    Just "pad"   -> Just PadDelivery
    Just "api"   -> Just APIDelivery
    _            -> Nothing

instance FromJSValue DocumentTag where
    fromJSValue = do
        name   <- fromJSValueField "name"
        value  <- fromJSValueField "value"
        case (name, value) of
             (Just n, Just v) -> return $ Just $ DocumentTag n v
             _ -> return Nothing  
    
instance FromJSValue Region where
    fromJSValue j =case fromJSValue j of -- Due to documentation inconsistency we need to support gb and en for a while.
      Just "se"    -> Just REGION_SE
      Just "en"    -> Just REGION_GB
      Just "gb"    -> Just REGION_GB
      _            -> Nothing

instance FromJSValue DraftData where
   fromJSValue = do
        title' <- fromJSValueField "title"
        invitationmessage <-  liftM join $ liftM (fmap nothingIfEmpty) $ fromJSValueField "invitationmessage"
        daystosign <- fromJSValueField "daystosign"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        signatories' <-  fromJSValueField "signatories"
        region' <- fromJSValueField "region"
        template' <- fromJSValueField "template"
        tags' <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValueM
        apicallbackurl' <- fromJSValueField "apicallbackurl"
        case (title', authentication', delivery') of
            (Just t, Just a, Just d) -> return $ Just DraftData {
                                      title =  t
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authentication = a
                                    , delivery = d
                                    , signatories = concat $ maybeToList $ signatories'
                                    , region = region'
                                    , template = joinB template'
                                    , tags = tags'
                                    , apicallbackurl = apicallbackurl'
                                 }
            _ -> return Nothing

applyDraftDataToDocument :: Kontrakcja m =>  Document -> DraftData -> Actor -> m (Either String Document)
applyDraftDataToDocument doc draft actor = do
    _ <- dbUpdate $ UpdateDraft (documentid doc) ( doc {
                                  documenttitle = title draft
                                , documentinvitetext = fromMaybe "" $ invitationmessage draft
                                , documentdaystosign = daystosign draft
                                , documentauthenticationmethod = authentication draft
                                , documentdeliverymethod = delivery draft
                                , documentregion = fromMaybe (documentregion doc) (region draft)
                                , documenttags = fromMaybe (documenttags doc) (fmap Set.fromList $ tags draft)
                                , documentapicallbackurl = (apicallbackurl draft)
                            }) actor
    when_ (template draft && (not $ isTemplate doc)) $ do
         dbUpdate $ TemplateFromDocument (documentid doc) actor
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

mergeSignatories :: SignatoryLink -> [SignatoryTMP] -> Maybe [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload, Maybe String)]
mergeSignatories docAuthor tmps =
        let (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstandDetails =  setFstname (getFirstName docAuthor) .
                                        setSndname (getLastName docAuthor) .
                                        setEmail   (getEmail docAuthor)
        in case (atmp) of
                ([authorTMP]) -> Just $ map toSignatoryDetails2 $ (setAuthorConstandDetails authorTMP) : notatmps
                _ -> Nothing
