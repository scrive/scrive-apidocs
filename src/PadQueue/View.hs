{-# LANGUAGE ExtendedDefaultRules #-}
module PadQueue.View (padQueueStateJSON, padQueuePage, padQueueStateJSONNotLoggedIn)
    where

import Doc.DocStateData
import Templates.Templates
import Control.Applicative
import Text.JSON
import Control.Logic
import Text.JSON.Gen as JSON
import AppView
import Context


padQueueStateJSON :: Monad m => Bool -> Maybe (Document, SignatoryLink) -> m JSValue
padQueueStateJSON systemlogged mds = JSON.runJSONGenT $ do
  JSON.value "documentid" $ show <$> documentid <$> fst <$> mds
  JSON.value "signatorylinkid" $ show <$> signatorylinkid <$> snd <$> mds
  JSON.value "logged" $ "system" <| systemlogged |> "pad"

padQueueStateJSONNotLoggedIn :: Monad m => m JSValue
padQueueStateJSONNotLoggedIn = JSON.runJSONGenT $ do
  JSON.value "logged" $ JSNull

padQueuePage :: TemplatesMonad m => Context -> m String
padQueuePage ctx = do
    renderTemplate "padQueueCurrentPage" $ do
      standardPageFields ctx kontrakcja Nothing False

