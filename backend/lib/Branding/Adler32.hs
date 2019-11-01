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
  ad1 <- domainAdler32 $ ctx ^. #brandedDomain
  ad2 <- maybe (return "") userGroupUIAdler32 mugidandui
  ad3 <- do
    case contextUser ctx of
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
    $ [bd ^. #mailTheme, bd ^. #signviewTheme, bd ^. #serviceTheme, bd ^. #loginTheme]
  return
    $  adler32Text
    $  T.concat
    $  [ showt (bd ^. #id)
       , imageAdler32 (bd ^. #favicon)
       , bd ^. #participantColor1
       , bd ^. #participantColor2
       , bd ^. #participantColor3
       , bd ^. #participantColor4
       , bd ^. #participantColor5
       , bd ^. #participantColor6
       , bd ^. #draftColor
       , bd ^. #cancelledColor
       , bd ^. #initatedColor
       , bd ^. #sentColor
       , bd ^. #deliveredColor
       , bd ^. #openedColor
       , bd ^. #reviewedColor
       , bd ^. #signedColor
       , showt $ bd ^. #mailTheme
       , showt $ bd ^. #signviewTheme
       , showt $ bd ^. #serviceTheme
       , showt $ bd ^. #loginTheme
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
