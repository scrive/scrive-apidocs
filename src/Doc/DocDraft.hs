{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument
  ) where

import Doc.SignatoryTMP
import Doc.DocStateData
import Misc
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
import Util.MonadUtils
import Text.JSON.FromJSValue

data DraftData = DraftData {
      title :: String
    , invitationmessage :: Maybe String
    , daystosign :: Maybe Int
    , authorization :: IdentificationType
    , signatories :: [SignatoryTMP]
    , region :: Region
    , template :: Bool
    } deriving Show

instance FromJSValue IdentificationType where
    fromJSValue j = case fromJSValue j of
             Just "eleg" -> Just ELegitimationIdentification
             Just "pad" -> Just PadIdentification
             _ -> Just EmailIdentification

instance FromJSValue Region where
    fromJSValue j = do
         s <-fromJSValue j
         find (\r -> codeFromRegion r  == s) allValues

instance FromJSValue DraftData where
   fromJSValue = do
        title' <- fromJSValueField "title"
        invitationmessage <-  liftM join $ liftM (fmap nothingIfEmpty) $ fromJSValueField "invitationmessage"
        daystosign <- fromJSValueField "daystosign"
        authorization' <-  fromJSValueField "authorization"
        signatories' <-  fromJSValueField "signatories"
        region' <- fromJSValueField "region"
        template' <- fromJSValueField "template"
        case (title', authorization', region') of
            (Just t, Just a, Just r) -> return $ Just DraftData {
                                      title =  t
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authorization = a
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
                                , documentallowedidtypes = [authorization draft]
                                , documentregion = region draft
                            }) actor
    when_ (template draft && (not $ isTemplate doc)) $ do
         dbUpdate $ TemplateFromDocument (documentid doc) actor
    case (mergeSignatories (fromJust $ getAuthorSigLink doc) (signatories draft)) of
         Nothing   -> return $ Left "Problem with author details while sending draft"
         Just sigs -> dbUpdate $ ResetSignatoryDetails2 (documentid doc) sigs actor

mergeSignatories :: SignatoryLink -> [SignatoryTMP] -> Maybe [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)]
mergeSignatories docAuthor tmps =
        let (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstandDetails =  setFstname (getFirstName docAuthor) .
                                        setSndname (getLastName docAuthor) .
                                        setEmail   (getEmail docAuthor)
        in case (atmp) of
                ([authorTMP]) -> Just $ map toSignatoryDetails2 $ (setAuthorConstandDetails authorTMP) : notatmps
                _ -> Nothing
