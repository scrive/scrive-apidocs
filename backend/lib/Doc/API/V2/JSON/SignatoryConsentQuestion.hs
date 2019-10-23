module Doc.API.V2.JSON.SignatoryConsentQuestion
  ( unjsonSignatoryConsentModule
  , SignatoryConsentResponsesForSigning(..)
  , unjsonSignatoryConsentResponsesForSigning
  ) where

import Data.Unjson
import Database.PostgreSQL.PQTypes
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS

import Doc.API.V2.JSON.Utils
import Doc.DocStateData
import Doc.SignatoryConsentQuestionID

unjsonSignatoryConsentModule :: UnjsonDef (Maybe String, [SignatoryConsentQuestion])
unjsonSignatoryConsentModule =
  objectOf $ (,) <$> fieldOpt "title" fst "Consent module title" <*> fieldBy
    "questions"
    snd
    "Consent module questions"
    (arrayOf unjsonQuestion)

  where
    unjsonQuestion :: UnjsonDef SignatoryConsentQuestion
    unjsonQuestion =
      objectOf
        $    pure defaultSignatoryConsentQuestion
        <*   (fieldReadonly "id" scqID "Question ID")
        <**> (field "title" scqTitle "Title" <**> pure (\t q -> q { scqTitle = t }))
        <**> (    field "positive_option" scqPositiveOption "Text of the positive answer"
             <**> pure (\po q -> q { scqPositiveOption = po })
             )
        <**> (    field "negative_option" scqNegativeOption "Text of the negative answer"
             <**> pure (\no q -> q { scqNegativeOption = no })
             )
        <*   (fieldReadOnlyOpt "response" scqResponse "Response")
        <**> (    fieldOptBy "detailed_description"
                             scqDescription
                             "Additional detailed description"
                             unjsonDescription
             <**> pure (\d q -> q { scqDescription = d })
             )

    unjsonDescription :: UnjsonDef (String, String)
    unjsonDescription =
      objectOf $ (,) <$> field "title" fst "Description title" <*> field
        "text"
        snd
        "Description contents"

newtype SignatoryConsentResponsesForSigning
  = SignatoryConsentResponsesForSigning [(SignatoryConsentQuestionID, Bool)]
  deriving Show

instance PQFormat SignatoryConsentResponsesForSigning where
  pqFormat = pqFormat @(JSON BS.ByteString)

instance FromSQL SignatoryConsentResponsesForSigning where
  type PQBase SignatoryConsentResponsesForSigning = PQBase (JSON BS.ByteString)
  fromSQL mbase = do
    JSON s <- fromSQL mbase
    case Aeson.eitherDecode s of
      Left _ ->
        hpqTypesError $ "fromSQL (SignatoryConsentResponsesForSigning): can't parse json"
      Right ae -> case parse unjsonSignatoryConsentResponsesForSigning ae of
        (Result res []) -> return res
        (Result _ _) ->
          hpqTypesError
            "fromSQL (SignatoryConsentResponsesForSigning): can't parse SignatoryConsentResponsesForSigning"

instance ToSQL SignatoryConsentResponsesForSigning where
  type PQDest SignatoryConsentResponsesForSigning = PQDest (JSON BS.ByteString)
  toSQL s = toSQL
    (unjsonToByteStringLazy' (Options { pretty = False, indent = 0, nulls = True })
                             unjsonSignatoryConsentResponsesForSigning
                             s
    )

unjsonSignatoryConsentResponsesForSigning :: UnjsonDef SignatoryConsentResponsesForSigning
unjsonSignatoryConsentResponsesForSigning = unjsonInvmapR
  (flip Result [] . SignatoryConsentResponsesForSigning)
  (\(SignatoryConsentResponsesForSigning xs) -> xs)
  (arrayOf unjsonResponse)
  where
    unjsonResponse :: UnjsonDef (SignatoryConsentQuestionID, Bool)
    unjsonResponse =
      objectOf $ (,) <$> field "id" fst "Question ID" <*> field "response" snd "Response"
