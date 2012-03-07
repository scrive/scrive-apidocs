{-# OPTIONS_GHC -fno-warn-orphans #-}
module Doc.DocDraft (
    DraftData,
    applyDraftDataToDocument
  ) where

--import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString as BS
import Doc.SignatoryTMP 
import Util.JSON
import Doc.DocStateData
import Misc
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.UTF8 as BS
import Kontra
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Data.List
import User.Region
import Doc.Model
import DB.Classes
import EvidenceLog.Model
import Util.MonadUtils

data DraftData = DraftData {
      title :: String
    , functionality :: DocumentFunctionality
    , invitationmessage :: Maybe String
    , daystosign :: Maybe Int
    , authorization :: IdentificationType
    , signatories :: [SignatoryTMP]
    , region :: Region
    , template :: Bool
    } deriving Show
    
instance FromJSON DocumentFunctionality where
    fromJSValue j = case fromJSValue j of 
             Just "basic" -> Just BasicFunctionality
             Just "advanced" -> Just AdvancedFunctionality
             _ -> Nothing

instance FromJSON IdentificationType where
    fromJSValue j = case fromJSValue j of 
             Just "eleg" -> Just ELegitimationIdentification
             Just "pad" -> Just PadIdentification
             _ -> Just EmailIdentification
                 
instance FromJSON Region where
    fromJSValue j = do
         s <-fromJSValue j
         find (\r -> codeFromRegion r  == s) allValues

    
instance FromJSON DraftData where
   fromJSON = do
        title' <- fromJSONField "title"
        functionality' <- fromJSONField "functionality"
        invitationmessage <-  liftM join $ liftM (fmap nothingIfEmpty) $ fromJSONField "invitationmessage"
        daystosign <- fromJSONField "daystosign"
        authorization' <-  fromJSONField "authorization"
        signatories' <-  fromJSONField "signatories"
        region' <- fromJSONField "region"
        template' <- fromJSONField "template"
        case (title',functionality', authorization', region') of
            (Just t, Just f, Just a, Just r) -> return $ Just DraftData {
                                      title =  t
                                    , functionality = f
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authorization = a
                                    , signatories = concat $ maybeToList $ signatories'
                                    , region = r
                                    , template = joinB template'
                                 }
            _ -> return Nothing
        
        
        
applyDraftDataToDocument :: (Actor a) =>  Kontrakcja m =>  Document -> DraftData -> a -> m (Either String Document)
applyDraftDataToDocument doc draft actor = do
    _ <- runDB $ dbUpdate $ UpdateDraft (documentid doc) ( doc {
                                  documenttitle = BS.fromString $ title draft
                                , documentfunctionality = functionality draft
                                , documentinvitetext = maybe BS.empty BS.fromString $ invitationmessage draft
                                , documentdaystosign = daystosign draft
                                , documentallowedidtypes = [authorization draft]
                                , documentregion = region draft
                            }) actor
    when_ (template draft && (not $ isTemplate doc)) $ do
         runDBUpdate $ TemplateFromDocument (documentid doc) actor
    case (mergeSignatories (fromJust $ getAuthorSigLink doc) (signatories draft)) of
         Nothing   -> return $ Left "Problem with author details while sending draft"
         Just sigs -> runDBUpdate $ ResetSignatoryDetails2 (documentid doc) sigs actor

                            

                            
mergeSignatories :: SignatoryLink -> [SignatoryTMP] -> Maybe [(SignatoryDetails, [SignatoryRole], [SignatoryAttachment], Maybe CSVUpload)]
mergeSignatories docAuthor tmps = 
        let (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstandDetails =  setFstname (getFirstName docAuthor) . 
                                        setSndname (getLastName docAuthor) . 
                                        setEmail   (getEmail docAuthor) 
        in case (atmp) of
                ([authorTMP]) -> Just $ map toSignatoryDetails2 $ (setAuthorConstandDetails authorTMP) : notatmps
                _ -> Nothing 
