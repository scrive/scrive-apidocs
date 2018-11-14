{-# LANGUAGE ExtendedDefaultRules #-}
module Branding.Adler32 (
     imageAdler32
   , brandingAdler32
  ) where

import Control.Monad.Catch
import Data.ByteString.Lazy (toStrict)
import Data.Digest.Adler32
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC8

import BrandedDomain.BrandedDomain
import Context
import DB
import Theme.Model
import User.Model
import UserGroup.Types
import UserGroup.Model
import VersionTH

brandingAdler32 :: (MonadDB m, MonadThrow m) => Context -> Maybe (UserGroupID, UserGroupUI) -> m String
brandingAdler32 ctx mugidandui = do
  ad1 <- domainAdler32 $ get ctxbrandeddomain ctx
  ad2 <- maybe (return "") userGroupUIAdler32 mugidandui
  ad3 <- do
    case getContextUser ctx of
      Nothing -> return ""
      Just user -> do
        ug2 <- dbQuery . UserGroupGetByUserID . userid $ user
        userGroupUIAdler32 (get ugID ug2, get ugUI ug2)
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [ad1,ad2,ad3,show versionID]


imageAdler32 :: BSC8.ByteString -> String
imageAdler32 image = BSC8.unpack $ adler32BS $ image

domainAdler32:: (MonadDB m, MonadThrow m) => BrandedDomain -> m String
domainAdler32 bd = do
  themesMD5 <- dbQuery $ GetThemesMD5 $ [ get bdMailTheme bd
                                        , get bdSignviewTheme bd
                                        , get bdServiceTheme bd
                                        , get bdLoginTheme bd ]
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [
      show         (get bdid      bd)
    , imageAdler32 (get bdFavicon bd)
    , get bdParticipantColor1     bd
    , get bdParticipantColor2     bd
    , get bdParticipantColor3     bd
    , get bdParticipantColor4     bd
    , get bdParticipantColor5     bd
    , get bdParticipantColor6     bd
    , get bdDraftColor            bd
    , get bdCancelledColor        bd
    , get bdInitatedColor         bd
    , get bdSentColor             bd
    , get bdDeliveredColor        bd
    , get bdOpenedColor           bd
    , get bdReviewedColor         bd
    , get bdSignedColor           bd
    , show $ get bdMailTheme      bd
    , show $ get bdSignviewTheme  bd
    , show $ get bdServiceTheme   bd
    , show $ get bdLoginTheme     bd
    ] ++ themesMD5

userGroupUIAdler32 :: (MonadDB m, MonadThrow m) => (UserGroupID, UserGroupUI) -> m String
userGroupUIAdler32 (ugid, ugui) = do
  themesMD5 <- dbQuery . GetThemesMD5 . catMaybes . fmap (\getter -> get getter ugui) $ [uguiMailTheme, uguiSignviewTheme, uguiServiceTheme]
  return $ BSC8.unpack $ adler32BS $ BSC8.pack $ concat $ [
      show ugid
    , maybe "" imageAdler32 (get uguiFavicon ugui)
    , show (get uguiMailTheme ugui)
    , show (get uguiSignviewTheme ugui)
    , show (get uguiServiceTheme ugui)
    ] ++ themesMD5

adler32BS :: BSC8.ByteString -> BSC8.ByteString
adler32BS = B16.encode . toStrict . Binary.encode . adler32
