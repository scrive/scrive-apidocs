module Doc.Data.SignatoryConsentQuestion (
    SignatoryConsentQuestion(..)
  , signatoryConsentQuestionsSelectors
  ) where

import Data.Data
import Data.Default
import Database.PostgreSQL.PQTypes

import Doc.SignatoryConsentQuestionID

data SignatoryConsentQuestion = SignatoryConsentQuestion {
    scqID             :: !SignatoryConsentQuestionID
  , scqTitle          :: !String
  , scqPositiveOption :: !String
  , scqNegativeOption :: !String
  , scqResponse       :: !(Maybe Bool)
  , scqDescription    :: !(Maybe (String, String))
  } deriving (Show, Typeable)

signatoryConsentQuestionsSelectors :: [SQL]
signatoryConsentQuestionsSelectors = [
    "signatory_link_consent_questions.id"
  , "signatory_link_consent_questions.title"
  , "signatory_link_consent_questions.positive_option"
  , "signatory_link_consent_questions.negative_option"
  , "signatory_link_consent_questions.response"
  , "signatory_link_consent_questions.description_title"
  , "signatory_link_consent_questions.description_text"
  ]

instance Default SignatoryConsentQuestion where
  def = SignatoryConsentQuestion {
      scqID             = unsafeSignatoryConsentQuestionID 0
    , scqTitle          = ""
    , scqPositiveOption = ""
    , scqNegativeOption = ""
    , scqResponse       = Nothing
    , scqDescription    = Nothing
    }

instance PQFormat SignatoryConsentQuestion where
  pqFormat _ = "%signatory_consent_question"

type instance CompositeRow SignatoryConsentQuestion = (SignatoryConsentQuestionID, String, String, String, Maybe Bool, Maybe String, Maybe String)

instance CompositeFromSQL SignatoryConsentQuestion where
  toComposite (scqid, title, positive_option, negative_option, response, description_title, description_text) = SignatoryConsentQuestion {
    scqID             = scqid
  , scqTitle          = title
  , scqPositiveOption = positive_option
  , scqNegativeOption = negative_option
  , scqResponse       = response
  , scqDescription    = (,) <$> description_title <*> description_text
  }
