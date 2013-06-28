{- Schemes of api callbacks. Each user can have at most one UserCallbackScheme. Most users don't have it set up at all.

   When documents are changed, and documentapicallbackurl attribute of document is set, we do a post request to this adress.
   This way external API users can update their state.

   UserCallbackScheme are extention of this mechanism per user.

    ConstantUrlScheme url        - if documentapicallbackurl is NOT set for document, parameter from this scheme will be used instead.
                                   Usefull when some user is using web interface for creating documents, but some system (i.e. external archive)
                                   is collecting data on what document have been signed by him.

    SalesforceScheme  oauthtoken - if documentapicallbackurl is set for document, some extra oauth details from this scheme will be added to callback request.

-}

module User.CallbackScheme.Model (
      UserCallbackScheme(..)
    , GetUserCallbackSchemeByUserID(..)
    , UpdateUserCallbackScheme(..)
  ) where

import DB
import User.Model
import Data.Data




data UserCallbackScheme =   ConstantUrlScheme String
                          | SalesforceScheme String
  deriving (Eq, Show, Data, Typeable)

$(jsonableDeriveConvertible [t| UserCallbackScheme |])


data GetUserCallbackSchemeByUserID = GetUserCallbackSchemeByUserID UserID
instance MonadDB m => DBQuery m GetUserCallbackSchemeByUserID (Maybe UserCallbackScheme) where
  query (GetUserCallbackSchemeByUserID uid) = do
    _ <- kRun $ selectUserCallbackSchemeSQL
      <> SQL "WHERE user_id = ?" [toSql uid]
    fetchUserCallbackScheme >>= oneObjectReturnedGuard >>= (return . (fmap snd))

data DeleteUserCallbackScheme = DeleteUserCallbackScheme UserID
instance MonadDB m => DBUpdate m DeleteUserCallbackScheme () where
  update (DeleteUserCallbackScheme uid) = do
    kRun01_ $ SQL "DELETE FROM user_callback_scheme WHERE (user_id = ?)"
             [toSql uid]

data UpdateUserCallbackScheme = UpdateUserCallbackScheme UserID UserCallbackScheme
instance MonadDB m => DBUpdate m UpdateUserCallbackScheme () where
  update (UpdateUserCallbackScheme uid callback) = do
    _ <- kRunRaw "LOCK TABLE user_callback_scheme IN ACCESS EXCLUSIVE MODE"
    kRun01_ $ SQL "DELETE FROM user_callback_scheme WHERE (user_id = ?)" [toSql uid]
    kRun01_ $ SQL ("INSERT INTO user_callback_scheme ("
      <> "  user_id"
      <> ", callback_scheme) VALUES (?, ?)")
      [ toSql uid
      , toSql callback
      ]


selectUserCallbackSchemeSQL :: SQL
selectUserCallbackSchemeSQL = SQL ("SELECT"
 <> "  user_id"
 <> ", callback_scheme"
 <> "  FROM user_callback_scheme"
 <> " ") []

fetchUserCallbackScheme :: MonadDB m => m [(UserID,UserCallbackScheme)]
fetchUserCallbackScheme = kFold decoder []
  where
    decoder acc userid callback_scheme = (userid,callback_scheme) : acc
