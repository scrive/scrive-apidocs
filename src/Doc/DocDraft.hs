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
--import Utils.Prelude
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
import File.FileID
import Control.Monad
import InputValidation
import Doc.SignatoryLinkID
import Data.Functor
import Utils.Default

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
    , authorattachments :: Maybe [FileID]
    } deriving Show

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


{-
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
        authorattachments' <- fromJSValueFieldCustom "authorattachments" $ fromJSValueCustomMany $ fmap (join . (fmap maybeRead)) $ (fromJSValueField "id")
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
                                    , authorattachments = authorattachments'
                                 }
            _ -> return Nothing
            -}


instance FromJSValueWithUpdate Document where
    fromJSValueWithUpdate mdoc = do
        title' <- fromJSValueField "title"
        invitationmessage <-  fromJSValueField "invitationmessage"
        daystosign' <- fromJSValueField "daystosign"
        authentication' <-  fromJSValueField "authentication"
        delivery' <-  fromJSValueField "delivery"
        signatories' <-  fromJSValueFieldCustom "signatories" (fromJSValueManyWithUpdate (fromMaybe [] $ documentsignatorylinks <$> mdoc))
        lang' <- fromJSValueField "lang"
        type' <- fmap (\t -> if t then Template else Signable) <$> fromJSValueField "template"
        tags' <- fromJSValueFieldCustom "tags" $ fromJSValueCustomMany  fromJSValueM
        apicallbackurl' <- fromJSValueField "apicallbackurl"
        authorattachments' <- fromJSValueFieldCustom "authorattachments" $ fromJSValueCustomMany $ fmap (join . (fmap maybeRead)) $ (fromJSValueField "id")
        return $ Just defaultValue {
            documenttitle = updateWithDefaultAndField "" documenttitle title',
            documentlang  = updateWithDefaultAndField LANG_SV documentlang lang',
            documentinvitetext = updateWithDefaultAndField "" documentinvitetext invitationmessage,
            documentdaystosign   = min 90 $ min 1 $ updateWithDefaultAndField 14 documentdaystosign daystosign',
            documentsignatorylinks = mapAuth authentication' $ mapDL delivery' $ updateWithDefaultAndField [] documentsignatorylinks signatories',
            documentauthorattachments = updateWithDefaultAndField [] documentauthorattachments (fmap AuthorAttachment <$> authorattachments'),
            documenttags = updateWithDefaultAndField Set.empty documenttags (Set.fromList <$> tags'),
            documenttype = updateWithDefaultAndField Signable documenttype type',
            documentapicallbackurl = updateWithDefaultAndField Nothing documentapicallbackurl apicallbackurl'
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



applyDraftDataToDocument :: Kontrakcja m =>  Document -> Document -> Actor -> m (Either String Document)
applyDraftDataToDocument doc draft actor = do
    if (documentstatus doc /= Preparation)
     then do
       Log.error $ "Document is not in preparation, is in " ++ show (documentstatus doc)
       return $ Left $ "applyDraftDataToDocument failed"
     else do
      _ <- dbUpdate $ UpdateDraft (documentid doc) ( doc {
                                    documenttitle = documenttitle draft
                                  , documentinvitetext = documentinvitetext draft
                                  , documentdaystosign = documentdaystosign draft
                                  , documentlang = documentlang draft
                                  , documenttags = documenttags draft
                                  , documentapicallbackurl = documentapicallbackurl draft
                              }) actor
      when_ (isTemplate draft && (not $ isTemplate doc)) $ do
           dbUpdate $ TemplateFromDocument (documentid doc) actor
      forM_ (documentauthorattachments doc) $ \att -> do
              when_ (not $ att `elem` (documentauthorattachments draft)) $ do
                dbUpdate $ RemoveDocumentAttachment (documentid doc) att actor

      case (mergeSignatories draft (documentsignatorylinks doc) (sort $ signatories draft)) of
           Nothing   -> return $ Left "Problem with author details while sending draft"
           Just sigs -> do
             mdoc <- runMaybeT $ do
               True <- dbUpdate $ ResetSignatoryDetails2 (documentid doc) sigs actor
               newdoc <- dbQuery $ GetDocumentByDocumentID $ documentid doc
               return newdoc
             return $ case mdoc of
               Nothing  -> Left "applyDraftDataToDocument failed"
               Just newdoc -> Right newdoc


matchSignatoriesAndSignatoriesTMPAndFixAuthor :: [SignatoryLink] -> [SignatoryTMP] -> Maybe [(Maybe SignatoryLink,SignatoryTMP)]
matchSignatoriesAndSignatoriesTMPAndFixAuthor sigs tmps =
          let
            (atmp, notatmps) = partition isAuthorTMP tmps
            (asig, notasigs) = partition isAuthor sigs
            matchSigs (s:ss) (t:ts) = if (Just (signatorylinkid s) == maybeSignatoryTMPid t)
                                          then (Just s,t) : matchSigs ss ts
                                          else matchSigs ss (t:ts)
            matchSigs [] (t:ts) = (Nothing,t) : matchSigs [] ts
            matchSigs _ _ = []
            setConstantDetails a =  setFstname (getFirstName a) .
                                    setSndname (getLastName a) .
                                    setEmail   (getEmail a) .
                                    setSignatoryTMPid (signatorylinkid a)
          in case (asig, atmp) of
               ([asig'], [atmp']) ->
                 if (Just (signatorylinkid asig') == maybeSignatoryTMPid atmp')
                   then Just $ (Just asig',setConstantDetails asig' atmp') : (matchSigs notasigs notatmps)
                   else Just $ (Nothing,setConstantDetails asig' atmp') : (matchSigs notasigs notatmps)
               _ -> Nothing

mergeSignatoryAndSignatoryTMP :: Maybe SignatoryLink -> SignatoryTMP -> SignatoryTMP
mergeSignatoryAndSignatoryTMP _  t = t

mergeSignatories :: DraftData
                 -> [SignatoryLink]
                 -> [SignatoryTMP]
                 -> Maybe [(Maybe SignatoryLinkID , SignatoryDetails, [SignatoryAttachment], Maybe CSVUpload, Maybe String,  Maybe String, AuthenticationMethod, DeliveryMethod)]
mergeSignatories docdraft sigs tmps =
        let
            tmps' = map (\(s,t) -> mergeSignatoryAndSignatoryTMP s t) <$> matchSignatoriesAndSignatoriesTMPAndFixAuthor sigs tmps
        in map toSignatoryDetails <$> tmps'



draftIsChangingDocument :: Document -> Document -> Bool
draftIsChangingDocument draft doc =
        (documenttitle draft /= documenttitle doc)
     || (documentinvitetext draft /= documentinvitetext doc)
     || (documentlang draft /= documentlang doc)
     || (documenttags draft /= documenttags doc)
     || (documentapicallbackurl draft /= documentapicallbackurl doc)
     || (isTemplate draft /= isTemplate doc)
     || (documentdaystosign draft /= documentdaystosign draf)
     || (draftIsChangingDocumentSignatories (documentsignatorylinks draf) (documentsignatorylinks doc))

draftIsChangingDocumentSignatories :: [SignatoryLink] -> [SignatoryLink] -> Bool
draftIsChangingDocumentSignatories (sl':sls') (sl:sls) = (newSignatorySignatoryLinkIsChangingSignatoryLink sl' sl) || (draftIsChangingDocumentSignatories sls' sls)
draftIsChangingDocumentSignatories [] [] = False
draftIsChangingDocumentSignatories _ _ = True


newSignatorySignatoryLinkIsChangingSignatoryLink :: SignatoryDetails -> SignatoryLink -> Bool
newSignatorySignatoryLinkIsChangingSignatoryLink newsl sl =
        (signatorydetails newsl /= signatorydetails sl)
     || (signatoryattachments newsl /= signatoryattachments sl)
     || (signatorylinkcsvupload newsl /= signatorylinkcsvupload sl)
     || (signatorylinksignredirecturl newsl /= signatorylinksignredirecturl sl)
     || (signatorylinkauthenticationmethod newsl /= signatorylinkauthenticationmethod sl)
     || (signatorylinkdeliverymethod newsl /= signatorylinkdeliverymethod sl)
