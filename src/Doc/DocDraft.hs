{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument
  ) where

import Control.Monad.Trans.Maybe
import Doc.SignatoryTMP
import Doc.DocStateData
import Utils.Enum
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

data DraftData = DraftData {
      title :: String
    , invitationmessage :: Maybe String
    , daystosign :: Maybe Int
    , authentication :: AuthenticationMethod
    , delivery :: DeliveryMethod
    , signatories :: [SignatoryTMP]
    , region :: Region
    , template :: Bool
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

instance FromJSValue Region where
    fromJSValue j = do
         s <-fromJSValue j
         find (\r -> codeFromRegion r  == s) allValues

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
        case (title', authentication', delivery', region') of
            (Just t, Just a, Just d, Just r) -> return $ Just DraftData {
                                      title =  t
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authentication = a
                                    , delivery = d
                                    , signatories = concat $ maybeToList $ signatories'
                                    , region = r
                                    , template = joinB template'
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
                                , documentregion = region draft
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
