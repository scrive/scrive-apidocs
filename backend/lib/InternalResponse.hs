module InternalResponse (
      InternalKontraResponse
    , eitherify
    , getFlashMessage
    , internalResponse
    , internalResponseWithFlash
    -- only for testing
    , isRedirect
    , hasFlashMessage
)where

import Happstack.Server (Response)
import qualified Data.Text as T

import FlashMessage
import KontraLink

data InternalKontraResponse =
  InternalKontraResponseWithFlash FlashMessage InternalKontraResponseContent |
  InternalKontraResponseWithoutFlash InternalKontraResponseContent

data InternalKontraResponseContent  =
  JustResponse Response     |
  JustKontraLink KontraLink |
  JustString String

eitherify :: InternalKontraResponse -> Either Response (Either KontraLink String )
eitherify (InternalKontraResponseWithFlash _ content)  = eitherifyContent content
eitherify (InternalKontraResponseWithoutFlash content) = eitherifyContent content

eitherifyContent :: InternalKontraResponseContent -> Either Response (Either KontraLink String)
eitherifyContent (JustResponse a)   = Left a
eitherifyContent (JustKontraLink a) = Right $ Left a
eitherifyContent (JustString a)     = Right $ Right a

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

instance CanBeInternalResponse String where
  internalResponseContent a = JustString a

instance CanBeInternalResponse Text where
  internalResponseContent a = JustString $ T.unpack a

isRedirect :: KontraLink -> InternalKontraResponse -> Bool
isRedirect l1 (InternalKontraResponseWithFlash _  (JustKontraLink l2)) = show l1 == show l2
isRedirect l1 (InternalKontraResponseWithoutFlash (JustKontraLink l2)) = show l1 == show l2
isRedirect _ _ = False

hasFlashMessage :: InternalKontraResponse -> Bool
hasFlashMessage a = isJust $ getFlashMessage a

getFlashMessage :: InternalKontraResponse -> Maybe FlashMessage
getFlashMessage (InternalKontraResponseWithFlash f _) = Just f
getFlashMessage _ = Nothing
