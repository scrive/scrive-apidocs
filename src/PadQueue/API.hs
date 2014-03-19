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
import Doc.DocumentMonad (withDocumentM, theDocument)
import API.Monad
import Control.Monad.Error
import qualified Log
import PadQueue.Model
import OAuth.Model
import Text.JSON.Gen
import Control.Exception.Lifted



padqueueAPI :: Route (KontraPlus Response)
padqueueAPI = choice
  [    dir "add"   $ hPostNoXToken $ toK2 $ addToQueue
     , dir "clear" $ hPostNoXToken $ toK0 $ clearQueue
  ]


-- PadQueue ACTIONS
addToQueue :: Kontrakcja m => DocumentID ->  SignatoryLinkID -> m Response
addToQueue did slid = api $ do
    (user, actor, _) <- getAPIUserWithPad APIDocSend
    dbQuery (GetDocumentByDocumentID did) `withDocumentM` do
      auid <- theDocument >>= \doc -> apiGuardJustM (serverError "No author found") $ return $ join $ maybesignatory <$> getAuthorSigLink doc
      when (not $ (auid == userid user)) $ do
          throwIO . SomeKontraException $ serverError "Permission problem. Not an author."
      sl <-  theDocument >>= \doc -> apiGuardJustM (serverError "Not a signatory") $ return $ getSigLinkFor slid doc
      if (signatorylinkdeliverymethod sl == PadDelivery)
          then do
              Log.mixlog_ $ "Adding signatory #" ++ (show slid) ++ "to padqueue of user #" ++ (show $ userid user)
              dbUpdate $ AddToPadQueue (userid user) slid actor
              runJSONGenT $ return ()
          else throwIO . SomeKontraException $ serverError "Not a pad document."

clearQueue :: Kontrakcja m => m Response
clearQueue = api $ do
    (user, actor, _) <- getAPIUserWithPad APIDocSend
    dbUpdate $ ClearPadQueue (userid user) actor
    runJSONGenT $ return ()
