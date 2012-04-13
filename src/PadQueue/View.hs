module PadQueue.View (padQueueStateJSON, padQueuePage, padQueueStateJSONNotLoggedIn)
    where
        
import Doc.DocStateData
import Templates.Templates
import Control.Applicative
import Text.JSON
import Control.Logic
import Text.JSON.Gen as JSON


padQueueStateJSON :: (TemplatesMonad m) => Bool -> Maybe (Document,SignatoryLink) -> m JSValue
padQueueStateJSON systemlogged mds = JSON.runJSONGenT $ do
        JSON.value "documentid" $ show <$> documentid <$> fst <$> mds
        JSON.value "signatorylinkid" $ show <$> signatorylinkid <$> snd <$> mds
        JSON.value "magichash" $ show  <$> signatorymagichash <$> snd <$> mds
        JSON.value "logged" $ "system" <| systemlogged |> "pad"
        
padQueueStateJSONNotLoggedIn :: (TemplatesMonad m) =>  m JSValue
padQueueStateJSONNotLoggedIn = JSON.runJSONGenT $ do
        JSON.value "logged" $ JSNull

padQueuePage :: (TemplatesMonad m) => m String
padQueuePage = renderTemplateFM "padQueueCurrentPage" $ return ()        