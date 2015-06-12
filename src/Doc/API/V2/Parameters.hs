{-# LANGUAGE GADTs #-}
module Doc.API.V2.Parameters (
    ApiV2Parameter(..)
  , ParameterOption(..)
  , apiV2Parameter
  , apiV2Parameter'
) where
import Happstack.Server.RqData
import Happstack.Server.Types
import KontraPrelude

import API.Monad.V2
import Kontra
import Data.Unjson
import Data.Unjson as Unjson
import DB
import Data.Text hiding (reverse, takeWhile)
import Happstack.Fields
import qualified Data.Aeson as Aeson
import Control.Exception.Lifted

-- | Parameters are either Obligatory or Optional
-- If they are Optional we may want a default value, or we may want Nothing
data ParameterOption a = Obligatory | Optional | OptionalWithDefault a

data ApiV2Parameter a where
  ApiV2ParameterBool  :: Text -> ParameterOption Bool -> ApiV2Parameter Bool
  ApiV2ParameterInt   :: Text -> ParameterOption Int -> ApiV2Parameter Int
  ApiV2ParameterText  :: Text -> ParameterOption Text -> ApiV2Parameter Text
  ApiV2ParameterJSON  :: Text -> ParameterOption a -> UnjsonDef a -> ApiV2Parameter a
  ApiV2ParameterInput :: Text -> ParameterOption Input -> ApiV2Parameter Input

-- | Same as `apiV2Parameter` except that it fails when we have a Nothing.
-- Given the same parameters it will behave the same way, but instead of giving
-- back a Just it will give back the value.
apiV2Parameter' :: Kontrakcja m => ApiV2Parameter a -> m a
apiV2Parameter' p = do
  v <- apiV2Parameter p
  case v of
    Just r -> return r
    Nothing -> throwIO . SomeKontraException $ requestParametersMissing [getParameterName p]

-- | Gets us all the different types of API parameters by matching proper
-- constructors on `ApiV2Parameter a` which includes `ParameterOption a`
apiV2Parameter :: Kontrakcja m => ApiV2Parameter a -> m (Maybe a)

apiV2Parameter (ApiV2ParameterInt name opt) = apiParameterUsingMaybeRead name opt
apiV2Parameter (ApiV2ParameterText name opt) = apiParameterUsingMaybeRead name opt

apiV2Parameter (ApiV2ParameterBool name opt) = do
  mValue <- getField $ unpack name
  case mValue of
    Just "true"  -> return $ Just True
    Just "false" -> return $ Just False
    Just _ -> throwIO . SomeKontraException $ requestParametersParseError name
    Nothing -> handleParameterOption name opt

apiV2Parameter (ApiV2ParameterJSON name opt jsonDef) = do
  mValue <- getFieldBS (unpack name)
  case mValue of
    Just paramValue -> case Aeson.eitherDecode paramValue of
      Left _ -> throwIO . SomeKontraException $ requestParametersParseError name
      Right paramAeson -> case (Unjson.parse jsonDef paramAeson) of
        (Result res []) -> return $ Just res
        (Result _ errs) -> throwIO . SomeKontraException $ requestParametersParseError (name `append` pack (show errs))
    Nothing -> handleParameterOption name opt

apiV2Parameter (ApiV2ParameterInput name opt) = do
  mValue <- getDataFn' (lookInput $ unpack name)
  case mValue of
    Just paramValue -> return $ Just paramValue
    Nothing -> handleParameterOption name opt

-- | Helper function for all parameters that can just be parsed using `maybeRead`
apiParameterUsingMaybeRead :: (Kontrakcja m, Read a) => Text -> ParameterOption a -> m (Maybe a)
apiParameterUsingMaybeRead name opt = do
  mValue <- getField $ unpack name
  case fmap maybeRead mValue of
    Just (Just v) -> return $ Just v
    Just Nothing  -> throwIO . SomeKontraException $ requestParametersParseError name
    Nothing -> handleParameterOption name opt

-- | Helper function to extract name from `ApiV2Parameter`
getParameterName :: ApiV2Parameter a -> Text
getParameterName (ApiV2ParameterBool n _) = n
getParameterName (ApiV2ParameterInt n _) = n
getParameterName (ApiV2ParameterText n _) = n
getParameterName (ApiV2ParameterJSON n _ _) = n
getParameterName (ApiV2ParameterInput n _) = n

-- | Helper function to handle when getting the parameter gives us `Nothing`
handleParameterOption :: Kontrakcja m => Text -> ParameterOption a -> m (Maybe a)
handleParameterOption _ Optional = return Nothing
handleParameterOption _ (OptionalWithDefault d) = return $ Just d
handleParameterOption n Obligatory = throwIO . SomeKontraException $ requestParametersMissing [n]
