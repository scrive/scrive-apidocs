module Util.FlashUtil where

import Kontra
import FlashMessage

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

class FlashMaybeString a where
  flashMaybeString :: a -> Kontra (Maybe String)

instance FlashMaybeString String where
  flashMaybeString s = return $ Just s
  
instance FlashMaybeString BS.ByteString where
  flashMaybeString bs = flashMaybeString (BS.toString bs)
  
instance (FlashMaybeString a) => FlashMaybeString (IO a) where
  flashMaybeString ioa = do
    ms <- liftIO $ ioa
    flashMaybeString ms
    
instance (FlashMaybeString a) => FlashMaybeString (Kontra a) where
  flashMaybeString ka = do
    ms <- ka
    flashMaybeString ms
    
instance (FlashMaybeString a) => FlashMaybeString (Maybe a) where
  flashMaybeString Nothing = return Nothing
  flashMaybeString (Just ms) = flashMaybeString ms
  
class IsFlash a where
  toFlash :: a -> Kontra (Maybe FlashMessage)
  
instance (FlashMaybeString a) => IsFlash (FlashType, a) where
  toFlash (ft, a) = do
    mmsg <- flashMaybeString a
    case mmsg of
      Nothing -> return Nothing
      Just msg -> return $ Just $ toFlashMsg ft msg
      
instance IsFlash FlashMessage where
  toFlash = return . Just
  
instance (IsFlash a) => IsFlash (Maybe a) where
  toFlash Nothing = return Nothing
  toFlash (Just fl) = toFlash fl
  
instance (IsFlash a) => IsFlash (Kontra a) where
  toFlash ka = do
    mFlash <- ka
    toFlash mFlash
    
instance (IsFlash a) => IsFlash (IO a) where
  toFlash ioa = do
    mFlash <- liftIO ioa
    toFlash mFlash
  
flashOperationFailed :: (FlashMaybeString a) => a -> Kontra ()
flashOperationFailed messageMonad = flash $ toFlash (OperationFailed, messageMonad)

flashOperationDone :: (FlashMaybeString a) => a -> Kontra ()
flashOperationDone messageMonad = flash $ toFlash (OperationDone, messageMonad)

flashSigningRelated :: (FlashMaybeString a) => a -> Kontra ()
flashSigningRelated messageMonad = flash $ toFlash (SigningRelated, messageMonad)
    
flash :: (IsFlash a) => a -> Kontra ()
flash flmsg = do
  mflash <- toFlash flmsg
  case mflash of
    Nothing -> return ()
    Just fflash -> addFlashMsg fflash
