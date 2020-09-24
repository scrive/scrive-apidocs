module PdfToolsLambda.Class
  ( SealSpec(..)
  , PreSealSpec(..)
  , AddImageSpec(..)
  , PadesSignSpec(..)
  , PdfToolsLambdaMonad(..)
  ) where

import Control.Monad.Trans
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL

import Doc.AddImageSpec
import Doc.SealSpec
import PdfToolsLambda.Conf

data PadesSignSpec = PadesSignSpec
  { inputFileContent   :: BS.ByteString
  , documentNumberText :: Text
  , overrideAPICredentials :: Maybe GlobalSignAPICredentials
  }

class Monad m => PdfToolsLambdaMonad m where
  callPdfToolsSealing    :: SealSpec       -> m (Maybe BS.ByteString)
  callPdfToolsPresealing :: PreSealSpec    -> m (Maybe BS.ByteString)
  callPdfToolsCleaning   :: BSL.ByteString -> m (Maybe BS.ByteString)
  callPdfToolsAddImage   :: AddImageSpec   -> m (Maybe BS.ByteString)
  callPdfToolsPadesSign  :: PadesSignSpec  -> m (Maybe BS.ByteString)
  callPdfToolsVerimiQesSetup :: VerimiQesSetupSpec -> m (Maybe BS.ByteString)
  callPdfToolsVerimiQesEvidence :: VerimiQesEvidenceSpec -> m (Maybe BS.ByteString)
  lambdaEnv              :: m PdfToolsLambdaEnv

-- | Generic, overlappable instance.
instance {-# OVERLAPPABLE #-}
  ( PdfToolsLambdaMonad m
  , Monad (t m)
  , MonadTrans t
  ) => PdfToolsLambdaMonad (t m) where
  callPdfToolsSealing           = lift . callPdfToolsSealing
  callPdfToolsPresealing        = lift . callPdfToolsPresealing
  callPdfToolsAddImage          = lift . callPdfToolsAddImage
  callPdfToolsPadesSign         = lift . callPdfToolsPadesSign
  callPdfToolsCleaning          = lift . callPdfToolsCleaning
  callPdfToolsVerimiQesSetup    = lift . callPdfToolsVerimiQesSetup
  callPdfToolsVerimiQesEvidence = lift . callPdfToolsVerimiQesEvidence
  lambdaEnv                     = lift lambdaEnv
