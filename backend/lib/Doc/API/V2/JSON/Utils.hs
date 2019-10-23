module Doc.API.V2.JSON.Utils (
  unjsonEnumBy
, unjsonEnum
, nothingToNullDef
, fieldReadOnlyOpt
) where

import Data.Tuple
import Data.Typeable
import Data.Unjson
import qualified Control.Applicative.Free as AltF
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

unjsonEnumBy :: forall  a . (Eq a) => Text -> [(a, Text)] -> UnjsonDef a
unjsonEnumBy desc enumDef = unjsonEnum desc parseEnum printEnum
  where
    parseEnum :: Text -> Maybe a
    parseEnum t = lookup t enumDef'

    enumDef' = map swap enumDef

    printEnum :: a -> Text
    printEnum a = fromMaybe err (lookup a enumDef)

    err = unexpectedError $ "Incomplete printEnum definition for" <+> desc

unjsonEnum :: Text -> (Text -> Maybe a) -> (a -> Text) -> UnjsonDef a
unjsonEnum desc parseEnum printEnum = SimpleUnjsonDef desc
                                                      parseFromEnumFromAeson
                                                      printEnumToAeson
  where
    printEnumToAeson a = Aeson.String $ printEnum a
    parseFromEnumFromAeson (Aeson.String s) = case (parseEnum s) of
      Just a -> Result a []
      _      -> fail $ T.unpack $ "cannot parse enum" <+> desc <+> "from" <+> s
    parseFromEnumFromAeson _ =
      fail $ T.unpack $ "cannot parse enum" <+> desc <+> "from not string"


-- | It is impossible to write a reasonable definition of 'Unjson (Maybe a)'
-- for any record like 'a' using field combinators.
-- So we use this to accomplish this goal.
nothingToNullDef :: UnjsonDef (Maybe a) -> UnjsonDef (Maybe a)
nothingToNullDef def =
  SimpleUnjsonDef "ReadOnly (Maybe a) for record like 'a'" (parse def) $ \mv -> case mv of
    Nothing -> Aeson.Null
    _       -> unjsonToJSON def mv

-- | Used for optional fields that are also read-only
fieldReadOnlyOpt
  :: (Unjson a, Typeable a) => Text -> (s -> Maybe a) -> Text -> AltF.Ap (FieldDef s) ()
fieldReadOnlyOpt name f desc = fieldReadonlyBy name f desc unjsonDefWithNull
  where
    unjsonDefWithNull :: Unjson a => UnjsonDef (Maybe a)
    unjsonDefWithNull =
      SimpleUnjsonDef "ReadOnly" (\v -> Just <$> parse unjsonDef v) $ \mv -> case mv of
        Nothing -> Aeson.Null
        Just v  -> unjsonToJSON unjsonDef v
