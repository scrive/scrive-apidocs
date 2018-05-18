module Doc.API.V2.JSON.Utils (
  unjsonEnumBy
, unjsonEnum
, nothingToNullDef
, fieldReadOnlyOpt
) where

import Control.Applicative.Free
import Data.Typeable
import Data.Unjson
import qualified Data.Aeson as Aeson
import qualified Data.Text as T

unjsonEnumBy :: (Eq a) => T.Text -> [(a,T.Text)] -> UnjsonDef a
unjsonEnumBy desc enumDef =
  unjsonEnum desc (parseEnum enumDef) (printEnum enumDef)
  where

    parseEnum :: [(a,T.Text)] -> T.Text -> Maybe a
    parseEnum ((m,t):ms) v = if (t == v)
                            then Just m
                            else parseEnum ms v
    parseEnum _ _ = Nothing

    printEnum :: (Eq a) => [(a,T.Text)] -> a -> T.Text
    printEnum ((m,t):ms) a =  if (a == m)
                            then t
                            else printEnum ms a
    printEnum _ _ = unexpectedError $
                    T.unpack ("Incomplete printEnum definition for" <+> desc)

unjsonEnum :: T.Text -> (T.Text -> Maybe a) -> (a -> T.Text) -> UnjsonDef a
unjsonEnum desc parseEnum printEnum  =
  SimpleUnjsonDef desc parseFromEnumFromAeson printEnumToAeson
  where
    printEnumToAeson a = Aeson.String $ printEnum a
    parseFromEnumFromAeson (Aeson.String s) =
      case (parseEnum s) of
        Just a -> Result a []
        _ -> fail $ T.unpack $ "cannot parse enum" <+> desc <+> "from" <+> s
    parseFromEnumFromAeson _ =
      fail $ T.unpack $ "cannot parse enum" <+> desc <+> "from not string"


-- | It is impossible to write a reasonable definition of 'Unjson (Maybe a)'
-- for any record like 'a' using field combinators.
-- So we use this to accomplish this goal.
nothingToNullDef :: UnjsonDef (Maybe a) ->  UnjsonDef (Maybe a)
nothingToNullDef def = SimpleUnjsonDef "ReadOnly (Maybe a) for record like 'a'"
  (parse def) $ \mv -> case mv of
                         Nothing -> Aeson.Null
                         _ -> unjsonToJSON def mv

-- | Used for optional fields that are also read-only
fieldReadOnlyOpt :: (Unjson a, Typeable a) =>
                    T.Text -> (s -> Maybe a) -> T.Text -> Ap (FieldDef s) ()
fieldReadOnlyOpt name f desc = fieldReadonlyBy name f desc unjsonDefWithNull
  where unjsonDefWithNull :: Unjson a => UnjsonDef (Maybe a)
        unjsonDefWithNull = SimpleUnjsonDef "ReadOnly"
          (\v -> Just <$> parse unjsonDef v) $ \mv ->
          case mv of
            Nothing -> Aeson.Null
            Just v -> unjsonToJSON unjsonDef v
