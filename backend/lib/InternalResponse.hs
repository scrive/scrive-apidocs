module InternalResponse (
      InternalKontraResponse(..)
    , eitherify
    , internalResponse
    , internalResponseWithFlash
    , isRedirect
    , isJSValue
    , hasFlashMessage
    , getFlashMessage
)where

import Happstack.Server (Response)
import Text.JSON (JSValue)

import FlashMessage
import KontraLink
import KontraPrelude

data InternalKontraResponse = 
  InternalKontraResponseWithFlash FlashMessage InternalKontraResponseContent |
  InternalKontraResponseWithoutFlash InternalKontraResponseContent
  
data InternalKontraResponseContent  = 
  JustResponse Response     |             
  JustKontraLink KontraLink |          
  JustJSValue JSValue       |     
  JustString String         |     
  JustEmpty              

eitherify :: InternalKontraResponse -> Either Response (Either KontraLink (Either JSValue (Either String ())))
eitherify (InternalKontraResponseWithFlash _ content)  = _eitherify content
eitherify (InternalKontraResponseWithoutFlash content) = _eitherify content

_eitherify :: InternalKontraResponseContent -> Either Response (Either KontraLink (Either JSValue (Either String ())))
_eitherify (JustResponse a)   = Left a
_eitherify (JustKontraLink a) = Right $ Left a
_eitherify (JustJSValue a)    = Right $ Right $ Left a
_eitherify (JustString a)     = Right $ Right $ Right $ Left a
_eitherify (JustEmpty)        = Right $ Right $ Right $ Right $ ()
    
class CanBeInternalResponse a where
  internalResponseContent :: a -> InternalKontraResponseContent

internalResponse :: (CanBeInternalResponse a) => a -> InternalKontraResponse
internalResponse a = InternalKontraResponseWithoutFlash $ internalResponseContent a

internalResponseWithFlash :: (CanBeInternalResponse a) => FlashMessage -> a -> InternalKontraResponse  
internalResponseWithFlash f a = InternalKontraResponseWithFlash f (internalResponseContent a)
  
instance CanBeInternalResponse Response where   
  internalResponseContent a = JustResponse a

instance CanBeInternalResponse KontraLink where   
  internalResponseContent a = JustKontraLink a
  
instance CanBeInternalResponse JSValue where   
  internalResponseContent a = JustJSValue a
  
instance CanBeInternalResponse String where   
  internalResponseContent a = JustString a  

instance CanBeInternalResponse () where   
  internalResponseContent () = JustEmpty  
  
isRedirect :: KontraLink -> InternalKontraResponse -> Bool  
isRedirect l1 (InternalKontraResponseWithFlash _  (JustKontraLink l2)) = show l1 == show l2
isRedirect l1 (InternalKontraResponseWithoutFlash (JustKontraLink l2)) = show l1 == show l2
isRedirect _ _ = False

isJSValue :: JSValue -> InternalKontraResponse -> Bool  
isJSValue jsv1 (InternalKontraResponseWithFlash _  (JustJSValue jsv2)) = jsv1 == jsv2
isJSValue jsv1 (InternalKontraResponseWithoutFlash (JustJSValue jsv2)) = jsv1 == jsv2
isJSValue _ _ = False

hasFlashMessage :: InternalKontraResponse -> Bool  
hasFlashMessage a = isJust $ getFlashMessage a

getFlashMessage :: InternalKontraResponse -> Maybe FlashMessage
getFlashMessage (InternalKontraResponseWithFlash f _) = Just f
getFlashMessage _ = Nothing
