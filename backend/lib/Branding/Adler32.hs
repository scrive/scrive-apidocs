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
import qualified Data.Text as T

import BrandedDomain.BrandedDomain
import Context
import DB
import Theme.Model
import User.Model
import UserGroup.Model
import UserGroup.Types
import VersionTH

brandingAdler32
  :: (MonadDB m, MonadThrow m) => Context -> Maybe (UserGroupID, UserGroupUI) -> m Text
brandingAdler32 ctx mugidandui = do
  ad1 <- domainAdler32 $ ctxBrandedDomain ctx
  ad2 <- maybe (return "") userGroupUIAdler32 mugidandui
  ad3 <- do
    case getContextUser ctx of
      Nothing   -> return ""
      Just user -> do
        ug <- dbQuery . UserGroupGetByUserID . userid $ user
        userGroupUIAdler32 (ugID ug, ugUI ug)
  return $ adler32Text $ T.concat $ [ad1, ad2, ad3, showt versionID]


imageAdler32 :: BSC8.ByteString -> Text
imageAdler32 image = T.pack $ BSC8.unpack $ adler32BS $ image

domainAdler32 :: (MonadDB m, MonadThrow m) => BrandedDomain -> m Text
domainAdler32 bd = do
  themesMD5 <-
    dbQuery
    $ GetThemesMD5
    $ [ bd ^. #bdMailTheme
      , bd ^. #bdSignviewTheme
      , bd ^. #bdServiceTheme
      , bd ^. #bdLoginTheme
      ]
  return
    $  adler32Text
    $  T.concat
    $  [ showt (bd ^. #bdid)
       , imageAdler32 (bd ^. #bdFavicon)
       , bd ^. #bdParticipantColor1
       , bd ^. #bdParticipantColor2
       , bd ^. #bdParticipantColor3
       , bd ^. #bdParticipantColor4
       , bd ^. #bdParticipantColor5
       , bd ^. #bdParticipantColor6
       , bd ^. #bdDraftColor
       , bd ^. #bdCancelledColor
       , bd ^. #bdInitatedColor
       , bd ^. #bdSentColor
       , bd ^. #bdDeliveredColor
       , bd ^. #bdOpenedColor
       , bd ^. #bdReviewedColor
       , bd ^. #bdSignedColor
       , showt $ bd ^. #bdMailTheme
       , showt $ bd ^. #bdSignviewTheme
       , showt $ bd ^. #bdServiceTheme
       , showt $ bd ^. #bdLoginTheme
       ]
    <> themesMD5

userGroupUIAdler32 :: (MonadDB m, MonadThrow m) => (UserGroupID, UserGroupUI) -> m Text
userGroupUIAdler32 (ugid, ugui) = do
  themesMD5 <-
    dbQuery
    . GetThemesMD5
    . catMaybes
    . fmap ($ ugui)
    $ [uguiMailTheme, uguiSignviewTheme, uguiServiceTheme]
  return
    $  adler32Text
    $  T.concat
    $  [ showt ugid
       , maybe "" imageAdler32 (uguiFavicon ugui)
       , showt (uguiMailTheme ugui)
       , showt (uguiSignviewTheme ugui)
       , showt (ugui)
       ]
    <> themesMD5

adler32Text :: Text -> Text
adler32Text = T.pack . BSC8.unpack . adler32BS . BSC8.pack . T.unpack

adler32BS :: BSC8.ByteString -> BSC8.ByteString
adler32BS = B16.encode . toStrict . Binary.encode . adler32
