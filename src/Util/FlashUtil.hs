-----------------------------------------------------------------------------
-- |
-- Module      :  Util.FlashUtil
-- Author      :  Eric Normand
-- Stability   :  development
-- Portability :  portable
--
-- Utility for making flash messages more friendly.
-----------------------------------------------------------------------------

module Util.FlashUtil ( flashOperationFailed
                      , flashOperationDone
                      , flashSigningRelated
                      , flash
                      ) where

import Kontra
import FlashMessage

import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS

{- |
   Anything that can be used as a flash message string.
 -}
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
  
{- |
   Anything that can become a FlashMessage.
 -}
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
  
{- |
   Flash a message of type OperationFailed.
 -}
flashOperationFailed :: (FlashMaybeString a) => a -> Kontra ()
flashOperationFailed messageMonad = flash $ toFlash (OperationFailed, messageMonad)

{- |
   Flash a message of type OperationDone.
-}
flashOperationDone :: (FlashMaybeString a) => a -> Kontra ()
flashOperationDone messageMonad = flash $ toFlash (OperationDone, messageMonad)

{- |
   Flash a message of type SigningRelated.
-}
flashSigningRelated :: (FlashMaybeString a) => a -> Kontra ()
flashSigningRelated messageMonad = flash $ toFlash (SigningRelated, messageMonad)
    
{- |
   Flash a message.
 -}
flash :: (IsFlash a) => a -> Kontra ()
flash flmsg = do
  mflash <- toFlash flmsg
  case mflash of
    Nothing -> return ()
    Just fflash -> addFlashMsg fflash
