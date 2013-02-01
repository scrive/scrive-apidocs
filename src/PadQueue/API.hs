module PadQueue.API (
    padqueueAPI
  ) where

import Happstack.StaticRouting
import KontraMonad
import Happstack.Server.Types
import Routing
import Doc.DocStateData
import Doc.Model
import Control.Applicative




import Util.SignatoryLinkUtils
import DB
import Kontra
import User.Model
import Doc.SignatoryLinkID
import Doc.DocumentID
import API.Monad
import Control.Monad.Error
import qualified Log
import PadQueue.Model
import OAuth.Model
import Text.JSON.Gen



padqueueAPI :: Route (KontraPlus Response)
padqueueAPI = choice
  [    dir "add"   $ hPostNoXToken $ toK2 $ addToQueue
     , dir "clear" $ hPostNoXToken $ toK0 $ clearQueue
  ]


-- PadQueue ACTIONS
addToQueue :: Kontrakcja m => DocumentID ->  SignatoryLinkID -> m Response
addToQueue did slid = api $ do
    (user, actor, _) <- getAPIUserWithPad APIDocSend
    doc <- apiGuardL (serverError "No document found") $ dbQuery $ GetDocumentByDocumentID $ did
    auid <- apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
    when (not $ (auid == userid user)) $ do
        throwError $ serverError "Permission problem. Not an author."
    _ <-  apiGuardJustM (serverError "Not a signatory") $ return $ getSigLinkFor doc slid
    if (documentdeliverymethod doc == PadDelivery)
        then do
            Log.debug $ "Adding signatory #" ++ (show slid) ++ "to padqueue of user #" ++ (show $ userid user)
            dbUpdate $ AddToPadQueue (userid user) did slid actor
            runJSONGenT $ return ()
        else throwError $ serverError "Not a pad document."

clearQueue :: Kontrakcja m => m Response
clearQueue = api $ do
    (user, actor, _) <- getAPIUserWithPad APIDocSend
    dbUpdate $ ClearPadQueue (userid user) actor
    runJSONGenT $ return ()
