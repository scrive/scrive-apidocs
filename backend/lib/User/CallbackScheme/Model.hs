{- Schemes of api callbacks. Each user can have at most one UserCallbackScheme. Most users don't have it set up at all.

   When documents are changed, and documentapicallbackurl attribute of document is set, we do a post request to this adress.
   This way external API users can update their state.

   UserCallbackScheme are extension of this mechanism per user.

    ConstantUrlScheme url        - if documentapicallbackurl is NOT set for document, parameter from this scheme will be used instead.
                                   Useful when some user is using web interface for creating documents, but some system (i.e. external archive)
                                   is collecting data on what document have been signed by him.

    SalesforceScheme  oauthtoken - if documentapicallbackurl is set for document, some extra oauth details from this scheme will be added to callback request.

-}

module User.CallbackScheme.Model (
      UserCallbackScheme(..)
    , DeleteUserCallbackScheme(..)
    , GetUserCallbackSchemeByUserID(..)
    , UpdateUserCallbackScheme(..)
  ) where

import Control.Monad.Catch
import Data.Data

import DB
import User.Model

data UserCallbackScheme = ConstantUrlScheme Text
                        | SalesforceScheme Text
                        | ConstantUrlSchemeV2 Text
                        | BasicAuthScheme Text Text -- Sample of DB value: {"BasicAuthScheme" : ["a","b"]}
                        | OAuth2Scheme Text Text Text Text -- Sample of DB value: {"OAuth2Scheme" : ["name","secret","url","scope"]}
                        | Hi3GScheme Text Text Text Text -- Hi3G's homegrown OAuth2. Sample of DB value: {"Hi3GScheme" : ["name","secret","url","scope"]}

  deriving (Eq, Show, Data, Typeable)

instance PQFormat UserCallbackScheme where
  pqFormat = pqFormat @String
instance FromSQL UserCallbackScheme where
  type PQBase UserCallbackScheme = PQBase String
  fromSQL = jsonFromSQL
instance ToSQL UserCallbackScheme where
  type PQDest UserCallbackScheme = PQDest String
  toSQL = jsonToSQL

data GetUserCallbackSchemeByUserID = GetUserCallbackSchemeByUserID UserID
instance (MonadDB m, MonadThrow m) => DBQuery m GetUserCallbackSchemeByUserID (Maybe UserCallbackScheme) where
  query (GetUserCallbackSchemeByUserID uid) = do
    runQuery_ . sqlSelect "user_callback_scheme" $ do
      sqlResult "callback_scheme"
      sqlWhereEq "user_id" uid
    fetchMaybe runIdentity

data DeleteUserCallbackScheme = DeleteUserCallbackScheme UserID
instance (MonadDB m, MonadThrow m) => DBUpdate m DeleteUserCallbackScheme () where
  update (DeleteUserCallbackScheme uid) = do
    runQuery01_ $ "DELETE FROM user_callback_scheme WHERE user_id =" <?> uid

data UpdateUserCallbackScheme = UpdateUserCallbackScheme UserID UserCallbackScheme
instance (MonadDB m, MonadThrow m) => DBUpdate m UpdateUserCallbackScheme () where
  update (UpdateUserCallbackScheme uid callback) = do
    runSQL_ "LOCK TABLE user_callback_scheme IN ACCESS EXCLUSIVE MODE"
    runQuery01_ $ "DELETE FROM user_callback_scheme WHERE user_id =" <?> uid
    runQuery01_ . sqlInsert "user_callback_scheme" $ do
      sqlSet "user_id" uid
      sqlSet "callback_scheme" callback
