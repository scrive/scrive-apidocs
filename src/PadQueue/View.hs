module PadQueue.View (showPadQueueCurrentPage)
    where
        
import Doc.DocStateData
import Templates.Templates
import Control.Applicative


showPadQueueCurrentPage :: (TemplatesMonad m) => Maybe (Document,SignatoryLink) -> m String
showPadQueueCurrentPage mds = do
    renderTemplateFM "padQueueCurrentPage" $ do
        field "documentid" $ show <$> documentid <$> fst <$> mds
        field "signatorylinkid" $ show <$> signatorylinkid <$> snd <$> mds
        field "magichash" $ show <$> signatorymagichash <$> snd <$> mds