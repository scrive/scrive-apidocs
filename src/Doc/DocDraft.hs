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
import Data.Functor
import qualified Data.ByteString.UTF8 as BS
import Kontra
import Util.SignatoryLinkUtils
import Util.HasSomeUserInfo
import Data.List
import User.Region
import Doc.Model
import DB.Classes

data DraftData = DraftData {
      title :: String
    , functionality :: DocumentFunctionality
    , invitationmessage :: Maybe String
    , daystosign :: Maybe Int
    , authorization :: IdentificationType
    , signatories :: [SignatoryTMP]
    , region :: Region
    } deriving Show
    
instance FromJSON DocumentFunctionality where
    fromJSValue j = case fromJSValue j of 
             Just "basic" -> Just BasicFunctionality
             Just "advanced" -> Just AdvancedFunctionality
             _ -> Nothing

instance FromJSON IdentificationType where
    fromJSValue j = case fromJSValue j of 
             Just "eleg" -> Just ELegitimationIdentification
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
        case (title',functionality', authorization', region') of
            (Just t, Just f, Just a, Just r) -> return $ Just DraftData {
                                      title =  t
                                    , functionality = f
                                    , invitationmessage = invitationmessage
                                    , daystosign = daystosign
                                    , authorization = a
                                    , signatories = concat $ maybeToList $ signatories'
                                    , region = r
                                 }
            _ -> return Nothing
        
        
        
applyDraftDataToDocument :: Kontrakcja m =>  Document -> DraftData -> m (Either String Document)
applyDraftDataToDocument doc draft = do
    time <- ctxtime <$> getContext
    _ <- runDBUpdate $ UpdateDraft (documentid doc) ( doc {
                                  documenttitle = BS.fromString $ title draft
                                , documentfunctionality = functionality draft
                                , documentinvitetext = maybe BS.empty BS.fromString $ invitationmessage draft
                                , documentdaystosign = daystosign draft
                                , documentallowedidtypes = [authorization draft]
                                , documentsignatoryattachments = concat $ map getAttachments $ signatories draft
                                , documentregion = region draft
                            }) time                            
    case (mergeSignatories (fromJust $ getAuthorSigLink doc) (signatories draft)) of 
         Nothing   -> return $ Left "Problem with author details while sending draft"
         Just sigs -> do
             res <- runDBUpdate $ ResetSignatoryDetails2 (documentid doc) sigs time             
             return res
             
                            

                            
mergeSignatories :: SignatoryLink -> [SignatoryTMP] -> Maybe [(SignatoryDetails, [SignatoryRole], Maybe CSVUpload)]
mergeSignatories docAuthor tmps = 
        let (atmp, notatmps) = partition isAuthorTMP tmps
            setAuthorConstandDetails =  setFstname (getFirstName docAuthor) . 
                                        setSndname (getLastName docAuthor) . 
                                        setEmail   (getEmail docAuthor) 
        in case (atmp) of
                ([authorTMP]) -> Just $ map toSignatoryDetails2 $ (setAuthorConstandDetails authorTMP) : notatmps
                _ -> Nothing 
