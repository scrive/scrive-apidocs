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
          , getSignatoryTMP
          , toSignatoryDetails1
          , getFiles
          , api_document_tag
          , api_signatory
          , api_file
        ) where


import Doc.DocStateData
import Text.JSON
import MinutesTime
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.ByteString.Base64 as Base64 
import API.API
import Doc.DocStorage
import Doc.DocUtils
import Kontra
import Misc
import Data.Maybe
import Data.Functor
import Control.Monad
import DB
import Util.JSON
import User.Lang
import User.Region
import Doc.JSON()
import User.Locale
import Doc.SignatoryTMP

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
    [("relation",showJSON $ fromSafeEnumInt $ signatoryroles sl)]
    ++
    [("fields", JSArray $ for (filterCustomField $ signatoryfields $ signatorydetails sl) $ \(s, label, _) -> JSObject $ toJSObject [
        ("name",  JSString $ toJSString label)
      , ("value", JSString $ toJSString $ sfValue s)
      ]
    )]
    where
      sfToJS sf name = (name, showJSON $ sfValue sf)
      fields = for (filter isStandardField $ signatoryfields $ signatorydetails sl) $
        \sf -> case sfType sf of
          FirstNameFT      -> sfToJS sf "fstname"
          LastNameFT       -> sfToJS sf "sndname"
          CompanyFT        -> sfToJS sf "company"
          CompanyNumberFT  -> sfToJS sf "companynr"
          PersonalNumberFT -> sfToJS sf "personalnr"
          EmailFT          -> sfToJS sf "email"
          _                -> error "api_signatory: impossible happened"

api_document_tag :: DocumentTag -> JSValue
api_document_tag tag = JSObject $ toJSObject [
      ("name", showJSON $ tagname tag)
    , ("value", showJSON $ tagvalue tag)]

api_file :: String -> BS.ByteString -> JSValue
api_file name content = 
  let base64data = BS.toString (Base64.encode content) in
  JSObject $ toJSObject [ ("name", showJSON name)
                        , ("content", showJSON base64data)]

api_document_file_read :: (APIContext c, Kontrakcja m) => File -> APIFunction m c JSValue
api_document_file_read file = do
    content <- getFileContents file
    return $ api_file (filename file) content

api_document :: Maybe [JSValue] -> Document -> JSValue
api_document mfiles doc = JSObject $ toJSObject $ [
    ("document_id", showJSON  $ show $ documentid doc)
  , ("title", showJSON  $ documenttitle doc)
  , ("type", showJSON  $ fromSafeEnumInt $ documenttype doc)
  , ("state", showJSON  $ fromSafeEnumInt $ documentstatus doc)
  , ("involved", JSArray $ map api_signatory $ documentsignatorylinks doc)
  , ("tags", JSArray $ map api_document_tag $ documenttags doc)
  , ("authorization", showJSON  $ fromSafeEnumInt $ documentallowedidtypes doc)
  , ("mdate", api_date $ documentmtime doc)
  , ("edate", api_date $ recentDate doc)
  , ("locale", jsonFromLocale $ getLocale doc)
  ] ++ case mfiles of
  Nothing -> []
  Just files -> [("files", JSArray files)]


api_document_read :: (APIContext c, Kontrakcja m, MonadDB m) => Bool -> Document -> APIFunction m c JSValue
api_document_read False doc = do
  return $ api_document Nothing doc
api_document_read True doc = do
  files <- mapM api_document_file_read =<< getFilesByStatus doc
  return $ api_document (Just files) doc


api_date :: MinutesTime -> JSValue
api_date = showJSON  . showMinutesTimeForAPI


getSignatoryTMP :: (APIContext c, Kontrakcja m) => [SignatoryRole] -> APIFunction m c (Maybe SignatoryTMP)
getSignatoryTMP defaultRoles = do
    fstname'        <- fromJSONField "fstname"
    sndname'        <- fromJSONField "sndname"
    company'        <- fromJSONField "company"
    personalnumber' <- fromJSONField "personalnr"
    companynumber'  <- fromJSONField "companynr"
    email'          <- fromJSONField "email"
    relation'       <- fromJSONField "relation"
    fields' <- fromJSONLocal "fields" $ fromJSONLocalMap $ do
                                        name <- fromJSONField "name"
                                        value <- fromJSONField "value"
                                        return $ (, value) <$> name
    return $ Just $ 
     (setFstname <$> fstname') $^
     (setSndname <$> sndname') $^
     (setCompany <$> company') $^
     (setPersonalnumber <$> personalnumber') $^
     (setCompanynumber <$> companynumber') $^
     (setEmail <$> email') $^
     (map (\(n,v) -> setCustomField n (fromMaybe "" v)) (concat $ maybeToList fields')) $^^
     (applyRelation (fromMaybe defaultRoles $ join $ toSafeEnumInt <$> relation') emptySignatoryTMP )


applyRelation :: [SignatoryRole] -> SignatoryTMP -> SignatoryTMP
applyRelation (SignatoryAuthor  : r) = makeAuthor . (applyRelation r)
applyRelation (SignatoryPartner : r) = makeSigns  . (applyRelation r)
applyRelation _                      = id


-- High level commons. Used buy some simmilar API's, but not all of them
getFiles :: (APIContext c, Kontrakcja m) => APIFunction m c [(String, BS.ByteString)]
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
