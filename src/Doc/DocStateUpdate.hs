{-# OPTIONS_GHC -Wall -Werror #-}
module Doc.DocStateUpdate
    ( restartDocument
    , markDocumentSeen
    ) where
    
import DBError
import Doc.DocState
import Doc.DocUtils
import Kontra
import Misc
import Control.Monad.State (get)
import Happstack.State     (update)
import MinutesTime
import GHC.Word

{- |
   Mark document seen securely.
 -}
markDocumentSeen :: DocumentID 
                 -> SignatoryLinkID
                 -> MagicHash
                 -> MinutesTime.MinutesTime
                 -> GHC.Word.Word32
                 -> Kontra (Either String Document)
markDocumentSeen docid sigid mh time ipnum =
  update $ MarkDocumentSeen docid sigid mh time ipnum

{- |
   Securely 
 -}
restartDocument :: Document -> Kontra (Either DBError Document)
restartDocument doc@Document { documentid } = do
  Context { ctxtime
          , ctxipnumber
          , ctxmaybeuser
          } <- get
  case ctxmaybeuser of
    Nothing   -> return $ Left DBNotLoggedIn
    Just user -> case getAuthorSigLink doc of
      Just authorsiglink | isSigLinkForUser user authorsiglink -> do
        enewdoc <- update $ RestartDocument documentid user ctxtime ctxipnumber 
        case enewdoc of
          Right newdoc -> return $ Right newdoc
          _            -> return $ Left DBResourceNotAvailable
      _ -> return $ Left DBResourceNotAvailable
