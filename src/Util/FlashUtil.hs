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
  
flashOperationFailed :: (FlashMaybeString a) => a -> Kontra ()
flashOperationFailed messageMonad = do
  mmessage <- flashMaybeString messageMonad
  case mmessage of
    Nothing -> return ()
    Just message -> addFlashMsg $ toFlashMsg OperationFailed message
