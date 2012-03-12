module PadQueue.View (padQueueStateJSON, padQueuePage, padQueueStateJSONNotLoggedIn)
    where
        
import Doc.DocStateData
import Templates.Templates
import Control.Applicative
import Text.JSON
import Text.JSON.Fields as JSON (json)
import qualified Text.JSON.Fields as JSON (field)
import Control.Monad.Trans


padQueueStateJSON :: (TemplatesMonad m) => Maybe (Document,SignatoryLink) -> m JSValue
padQueueStateJSON mds = liftIO $ json $ do
        JSON.field "documentid" $ maybe JSNull (JSString . toJSString . show)  $ documentid <$> fst <$> mds
        JSON.field "signatorylinkid" $ maybe JSNull (JSString . toJSString . show)   $  signatorylinkid <$> snd <$> mds
        JSON.field "magichash" $ maybe JSNull (JSString . toJSString . show)   $ signatorymagichash <$> snd <$> mds
        JSON.field "logged" $ "true"
        
padQueueStateJSONNotLoggedIn :: (TemplatesMonad m) =>  m JSValue
padQueueStateJSONNotLoggedIn = liftIO $ json $ do
        JSON.field "logged" $ "false"

padQueuePage :: (TemplatesMonad m) => m String
padQueuePage = renderTemplateFM "padQueueCurrentPage" $ return ()        