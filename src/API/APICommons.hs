{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  API.APICommons
-- Maintainer  :  mariusz@skrivapa.se
-- Stability   :  development
-- Portability :  portable
--
-- Constants and data structures used by many other API's
-- Some low level helpers.
--
-- !! Constants described here should be used between all API's
-- There are constants for stuff like document type, document state, to say thet a signatory is viewer etc.
-- Don't invent them yourself. Don't return a 'human readable' string. Use stuff from here.
--
-- !! JSON priners (like the one api_document) should be shared as much as posible
--
-- Also if there is a common reader for more then one api
-- , like for files or signatories it should be put here
-----------------------------------------------------------------------------
module API.APICommons (
            api_document_read
          , api_document
          , SignatoryTMP(..)
          , getSignatoryTMP
          , mergeSignatoryWithTMP
          , toSignatoryDetails
          , getFiles
          , api_document_tag
          , api_signatory
          , api_file
          , toSignatoryRoles
          , emptySignatoryTMP
        ) where


import Doc.DocStateData
import Text.JSON
import MinutesTime
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as Base64 
import API.API
import Doc.DocStorage
import Doc.DocControl
import Doc.DocUtils
import Control.Monad.Trans
import Kontra
import Misc
import Data.Maybe
import Data.Foldable (fold)
import Data.Functor
import Control.Monad
import Util.SignatoryLinkUtils
import DB.Classes
import qualified AppLogger as Log ()
import Util.JSON
import User.Lang
import User.Region
import Doc.JSON()
import User.Locale

{- -}



data DOCUMENT_RELATION =
      DOCUMENT_RELATION_AUTHOR_SECRETARY
    | DOCUMENT_RELATION_AUTHOR_SIGNATORY
    | DOCUMENT_RELATION_SIGNATORY
    | DOCUMENT_RELATION_VIEWER
    | DOCUMENT_RELATION_OTHER

instance SafeEnum DOCUMENT_RELATION where
    fromSafeEnum DOCUMENT_RELATION_AUTHOR_SECRETARY = 1
    fromSafeEnum DOCUMENT_RELATION_AUTHOR_SIGNATORY = 2
    fromSafeEnum DOCUMENT_RELATION_SIGNATORY = 5
    fromSafeEnum DOCUMENT_RELATION_VIEWER = 10
    fromSafeEnum DOCUMENT_RELATION_OTHER = 20
    toSafeEnum 1  = Just DOCUMENT_RELATION_AUTHOR_SECRETARY
    toSafeEnum 2  = Just DOCUMENT_RELATION_AUTHOR_SIGNATORY
    toSafeEnum 5  = Just DOCUMENT_RELATION_SIGNATORY
    toSafeEnum 10 = Just DOCUMENT_RELATION_VIEWER
    toSafeEnum 20 = Just DOCUMENT_RELATION_OTHER
    toSafeEnum _  = Nothing

api_document_relation :: SignatoryLink -> DOCUMENT_RELATION
api_document_relation sl
    | isAuthor sl && isSignatory sl = DOCUMENT_RELATION_AUTHOR_SIGNATORY
    | isAuthor sl                   = DOCUMENT_RELATION_AUTHOR_SECRETARY
    | isSignatory sl                = DOCUMENT_RELATION_SIGNATORY
    | otherwise                     = DOCUMENT_RELATION_VIEWER

api_signatory :: SignatoryLink -> JSValue
api_signatory sl = JSObject $ toJSObject $ 
    fields
    ++
    case maybeseeninfo sl of
     Just seeninfo ->  [("seen", api_date $ signtime seeninfo)]
     Nothing -> []
    ++
    case maybesigninfo sl of
     Just signinfo ->  [("sign", api_date $ signtime signinfo)]
     Nothing -> []
    ++
    [("relation",showJSON $ fromSafeEnumInt $ api_document_relation sl)]
    ++
    [("fields", JSArray $ for (filterCustomField $ signatoryfields $ signatorydetails sl) $ \(s, label, _) -> JSObject $ toJSObject [
        ("name",  JSString $ toJSString $ BS.toString label)
      , ("value", JSString $ toJSString $ BS.toString (sfValue s))
      ]
    )]
    where
      sfToJS sf name = (name, showJSON $ BS.toString $ sfValue sf)
      fields = for (filter (not . isFieldCustom) $ signatoryfields $ signatorydetails sl) $
        \sf -> case sfType sf of
          FirstNameFT      -> sfToJS sf "fstname"
          LastNameFT       -> sfToJS sf "sndname"
          CompanyFT        -> sfToJS sf "company"
          CompanyNumberFT  -> sfToJS sf "companynr"
          PersonalNumberFT -> sfToJS sf "personalnr"
          EmailFT          -> sfToJS sf "email"
          CustomFT _ _     -> error "api_signatory: impossible happened"

api_document_tag :: DocumentTag -> JSValue
api_document_tag tag = JSObject $ toJSObject [
      ("name", showJSON $ BS.toString $ tagname tag)
    , ("value", showJSON $ BS.toString $ tagvalue tag)]
                       
api_file :: BS.ByteString -> BS.ByteString -> JSValue
api_file name content = 
  let base64data = BS.toString (Base64.encode content) in
  JSObject $ toJSObject [ ("name", showJSON $ BS.toString name)
                        , ("content", showJSON base64data)]

api_document_file_read :: (APIContext c, Kontrakcja m) => File -> APIFunction m c JSValue
api_document_file_read file = do
    ctx <- getContext
    content <- liftIO $ getFileContents ctx file
    return $ api_file (filename file) content

api_document :: Maybe [JSValue] -> Document -> JSValue
api_document mfiles doc = JSObject $ toJSObject $ [
  ("document_id", showJSON  $ show $ unDocumentID $ documentid doc)
  , ("title", showJSON  $ BS.toString $ documenttitle doc)
  , ("type", showJSON  $ fromSafeEnumInt $ documenttype doc)
  , ("state", showJSON  $ fromSafeEnumInt $ documentstatus doc)
  , ("involved", JSArray $ map api_signatory $ documentsignatorylinks doc)
  , ("tags", JSArray $ map api_document_tag $ documenttags doc)
  , ("authorization", showJSON  $ fromSafeEnumInt $ documentallowedidtypes doc)
  , ("mdate", api_date $ documentmtime doc)
  , ("locale", jsonFromLocale $ getLocale doc)
  ] ++ case mfiles of
  Nothing -> []
  Just files -> [("files", JSArray files)]


api_document_read :: (APIContext c, Kontrakcja m, DBMonad m) => Bool -> Document -> APIFunction m c JSValue
api_document_read False doc = do
  return $ api_document Nothing doc
api_document_read True doc = do
  files <- mapM api_document_file_read =<< getFilesByStatus doc
  return $ api_document (Just files) doc


api_date :: MinutesTime -> JSValue
api_date = showJSON  . showMinutesTimeForAPI


data SignatoryTMP = SignatoryTMP {
                fstname::Maybe BS.ByteString,
                sndname::Maybe BS.ByteString,
                company::Maybe BS.ByteString,
                personalnumber::Maybe BS.ByteString,
                companynumber::Maybe BS.ByteString,
                email::Maybe BS.ByteString,
                fields :: [(BS.ByteString,Maybe BS.ByteString)],
                relation :: Maybe Integer
            } deriving Show
                       
emptySignatoryTMP :: SignatoryTMP
emptySignatoryTMP = SignatoryTMP {
                fstname = Nothing,
                sndname = Nothing,
                company = Nothing,
                personalnumber = Nothing,
                companynumber = Nothing, 
                email = Nothing,
                fields = [],
                relation = Nothing
            }

getSignatoryTMP :: (APIContext c, Kontrakcja m) => APIFunction m c (Maybe SignatoryTMP)
getSignatoryTMP = do
    fstname        <- fromJSONField "fstname"
    sndname        <- fromJSONField "sndname"
    company        <- fromJSONField "company"
    personalnumber <- fromJSONField "personalnr"
    companynumber  <- fromJSONField "companynr"
    email          <- fromJSONField "email"
    relation       <- fromJSONField "relation"
    fields <- fromJSONLocal "fields" $ fromJSONLocalMap $ do
                                        name <- fromJSONField "name"
                                        value <- fromJSONField "value"
                                        return $ (, value) <$> name
    return $ Just $ SignatoryTMP
                { fstname = fstname
                , sndname = sndname
                , company = company
                , personalnumber = personalnumber
                , companynumber = companynumber
                , email = email
                , fields = concat $ maybeToList fields
                , relation = relation
                }

rolesFromRelation :: DOCUMENT_RELATION -> [SignatoryRole]
rolesFromRelation DOCUMENT_RELATION_AUTHOR_SECRETARY = [SignatoryAuthor]
rolesFromRelation DOCUMENT_RELATION_AUTHOR_SIGNATORY = [SignatoryAuthor, SignatoryPartner]
rolesFromRelation DOCUMENT_RELATION_SIGNATORY        = [SignatoryPartner]
rolesFromRelation DOCUMENT_RELATION_VIEWER           = []
rolesFromRelation DOCUMENT_RELATION_OTHER            = []


toSignatoryRoles :: SignatoryTMP -> Maybe [SignatoryRole]
toSignatoryRoles sTMP = case relation sTMP of
  Nothing -> Nothing
  Just bs -> rolesFromRelation <$> (toSafeEnum bs)
    

toSignatoryDetails :: SignatoryTMP -> SignatoryDetails
toSignatoryDetails sTMP =
    let sig = makeSignatory [] [] BS.empty
                 (fold $ fstname sTMP)
                 (fold $ sndname sTMP)
                 (fold $ email sTMP)
                 (SignOrder 1)
                 (fold $ company sTMP)
                 (fold $ personalnumber sTMP)
                 (fold $ companynumber sTMP)
    in sig { signatoryfields = signatoryfields sig ++ customfields }
    where
      customfields = for (fields sTMP) $ \(name, mvalue) ->
        SignatoryField {
            sfType = CustomFT name $ isJust mvalue
          , sfValue = fromMaybe BS.empty mvalue
          , sfPlacements = []
          }

mergeSignatoryWithTMP :: (APIContext c, Kontrakcja m) => SignatoryTMP -> SignatoryLink-> APIFunction m c SignatoryLink
mergeSignatoryWithTMP sTMP sl@(SignatoryLink{signatorydetails=sd}) = do
  return $ sl {
    signatorydetails = sd { signatoryfields = replaceValues $ signatoryfields sd }
  }
  where
    replaceValues = map $ \sf -> case sfType sf of
      FirstNameFT -> replace sf fstname
      LastNameFT -> replace sf sndname
      CompanyFT -> replace sf company
      CompanyNumberFT -> replace sf companynumber
      PersonalNumberFT -> replace sf personalnumber
      EmailFT -> replace sf email
      CustomFT label _ -> case join $ lookup label (fields sTMP) of
                            Just nv -> sf { sfValue = nv }
                            Nothing -> sf
      where
        replace sf f = sf { sfValue = fromMaybe (sfValue sf) (f sTMP) }

-- High level commons. Used buy some simmilar API's, but not all of them
getFiles :: (APIContext c, Kontrakcja m) => APIFunction m c [(BS.ByteString, BS.ByteString)]
getFiles = fmap (fromMaybe []) $ fromJSONLocal "files" $ fromJSONLocalMap $ do
    name    <- fromJSONField "name"
    content <- fromJSONFieldBase64 "content"
    when (isNothing name || isNothing content) $ throwApiError API_ERROR_MISSING_VALUE "Problems with files upload."
    return $ Just (fromJust name, fromJust content)

{- | JSON from Locale
 -}
jsonFromLocale :: Locale -> JSValue
jsonFromLocale l = JSObject $ toJSObject [("region", showJSON $ codeFromRegion $ getRegion l),
                                          ("language", showJSON $ codeFromLang $ getLang l)]

instance FromJSON Locale where
  fromJSValue (JSObject obj) = 
    case (fromJSValue =<< getJSONField "region" obj, fromJSValue =<< getJSONField "language" obj) of
      (Just region, Just language) -> Just $ mkLocale region language
      (Just region, _)             -> Just $ mkLocaleFromRegion region
      _                            -> Nothing
  fromJSValue _ = Nothing

instance FromJSON Region where
  fromJSValue a = regionFromCode =<< (fromJSValue a)
  
instance FromJSON Lang where
  fromJSValue a = langFromCode =<< (fromJSValue a)
