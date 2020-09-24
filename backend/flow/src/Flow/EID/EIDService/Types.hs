
module Flow.EID.EIDService.Types (
  EIDServiceTransactionFromDB(..)
  , UnifiedRedirectUrl(..)
  ) where

import Data.Time
import Prelude hiding (empty)
import qualified Data.Text as T

import EID.EIDService.Types hiding
  ( EIDServiceTransactionFromDB(..), UnifiedRedirectUrl(..)
  )
import Flow.Id
import Flow.Names
import Session.SessionID

data EIDServiceTransactionFromDB = EIDServiceTransactionFromDB
  { estID :: EIDServiceTransactionID
  , estStatus :: EIDServiceTransactionStatus
  , estInstanceId :: InstanceId
  , estUserName :: UserName
  , estAuthKind :: EIDServiceAuthenticationKind
  , estProvider :: EIDServiceTransactionProvider
  , estSessionID :: SessionID
  , estDeadline :: UTCTime
  } deriving (Show)

data UnifiedRedirectUrl = UnifiedRedirectUrl {
    redDomain :: Text
  , redProvider :: EIDServiceTransactionProvider
  , redAuthKind :: EIDServiceAuthenticationKind
  , redInstanceId :: InstanceId
  , redUserName :: UserName
  , redPostRedirectUrl :: Maybe Text
  }

instance Show UnifiedRedirectUrl where
  show UnifiedRedirectUrl {..} =
    intercalate
        "/"
        [ T.unpack redDomain
        , "eid-service-flow"
        , "redirect-endpoint"
        , T.unpack $ toRedirectURLName redProvider
        , show redInstanceId
        , show redUserName
        ]
      <> postRedirectFragment
    where
      postRedirectFragment
        | Just target <- redPostRedirectUrl = "?redirect=" <> T.unpack target
        | otherwise = ""
