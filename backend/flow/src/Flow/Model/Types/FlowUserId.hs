module Flow.Model.Types.FlowUserId
  ( FlowUserId(..)
  , flowUserIdToSeries
  ) where

import Data.Aeson
import GHC.Generics

import User.UserID
import qualified Flow.Model.Types.UserIdType as IdType

-- | Flow participant ID.
--
-- We currently do not have a proper data model for Flow participants.
-- In lieu of that we're reusing some of the data used
-- by kontrakcja documents and signatory links.
--
-- In particular, each participant can be identified
-- via their contact details or via a Scrive account user ID.
data FlowUserId
  = Email Text
  | PhoneNumber Text
  | UserId UserID
  deriving (Eq, Generic, Ord, Show)

instance FromJSON FlowUserId where
  parseJSON = withObject "FlowUserId" $ \v -> do
    idType <- v .: "id_type"
    case idType of
      IdType.Email       -> Email <$> v .: "id"
      IdType.PhoneNumber -> PhoneNumber <$> v .: "id"
      IdType.UserId      -> UserId <$> v .: "id"

instance ToJSON FlowUserId where
  toEncoding = pairs . flowUserIdToSeries

flowUserIdToSeries :: FlowUserId -> Series
flowUserIdToSeries = \case
  Email       e   -> "id_type" .= IdType.Email <> "id" .= e
  PhoneNumber pn  -> "id_type" .= IdType.PhoneNumber <> "id" .= pn
  UserId      uid -> "id_type" .= IdType.UserId <> "id" .= uid
