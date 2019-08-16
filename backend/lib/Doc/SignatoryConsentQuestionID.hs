module Doc.SignatoryConsentQuestionID (
    SignatoryConsentQuestionID
  , unsafeSignatoryConsentQuestionID
  , fromSignatoryConsentQuestionID
  ) where

import Data.Binary as B
import Data.Data
import Data.Int
import Data.Unjson
import Database.PostgreSQL.PQTypes
import Happstack.Server
import qualified Data.Text as T

import Log.Identifier

newtype SignatoryConsentQuestionID = SignatoryConsentQuestionID Int64
  deriving (Eq, Ord, Typeable, Data)
deriving newtype instance Read SignatoryConsentQuestionID
deriving newtype instance Show SignatoryConsentQuestionID

instance PQFormat SignatoryConsentQuestionID where
  pqFormat = pqFormat @Int64

instance FromReqURI SignatoryConsentQuestionID where
  fromReqURI = maybeRead . T.pack

deriving newtype instance Binary SignatoryConsentQuestionID
deriving newtype instance FromSQL SignatoryConsentQuestionID
deriving newtype instance ToSQL SignatoryConsentQuestionID
deriving newtype instance Unjson SignatoryConsentQuestionID

instance Identifier SignatoryConsentQuestionID where
  idDefaultLabel                         = "signatory_consent_question_id"
  idValue (SignatoryConsentQuestionID k) = int64AsStringIdentifier k

unsafeSignatoryConsentQuestionID :: Int64 -> SignatoryConsentQuestionID
unsafeSignatoryConsentQuestionID = SignatoryConsentQuestionID

fromSignatoryConsentQuestionID :: SignatoryConsentQuestionID -> Int64
fromSignatoryConsentQuestionID (SignatoryConsentQuestionID did) = did
