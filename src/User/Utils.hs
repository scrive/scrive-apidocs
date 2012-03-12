module User.Utils where

import Control.Monad.State
import Data.Functor
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.UTF8 as BS

import Crypto.RNG (CryptoRNG)
import DB.Classes
import Doc.DocStateData
import Company.Model
import Kontra
import KontraLink
import Misc
import User.Model
import Util.SignatoryLinkUtils
import Util.MonadUtils

{- |
    This looks up the company for the given user, if the user doesn't
    have a company then it returns Nothing.
-}
getCompanyForUser :: DBMonad m => User -> m (Maybe Company)
getCompanyForUser = maybe (return Nothing) (runDBQuery . GetCompany) . usercompany

-- | Version to be executed within DB monad (currently used in tests only)
getCompanyForUser' :: User -> DB (Maybe Company)
getCompanyForUser' = maybe (return Nothing) (dbQuery . GetCompany) . usercompany

{- |
   Guard against a POST with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserPost :: Kontrakcja m => m KontraLink -> m KontraLink
withUserPost action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
         Just _  -> action
         Nothing -> return $ LinkLogin (ctxlocale ctx) NotLogged

{- |
   Guard against a GET with no logged in user.
   If they are not logged in, redirect to login page.
-}
withUserGet :: Kontrakcja m => m a -> m (Either KontraLink a)
withUserGet action = do
  ctx <- getContext
  case ctxmaybeuser ctx of
    Just _  -> Right <$> action
    Nothing -> return $ Left $ LinkLogin (ctxlocale ctx) NotLogged

{- |
     Takes a document and a action
     Runs an action only if current user (from context) is author of document
| -}
withDocumentAuthor :: Kontrakcja m => Document -> m a -> m a
withDocumentAuthor document action = do
  ctx <- getContext
  user <- guardJust $ ctxmaybeuser ctx
  sl <- guardJust $ getAuthorSigLink document
  guard $ isSigLinkFor user sl
  action

{- |
   Guard against a GET with logged in users who have not signed the TOS agreement.
   If they have not, redirect to their account page.
-}
checkUserTOSGet :: Kontrakcja m => m a -> m (Either KontraLink a)
checkUserTOSGet action = do
    ctx <- getContext
    case ctxmaybeuser ctx of
        Just (User{userhasacceptedtermsofservice = Just _}) -> Right <$> action
        Just _ -> return $ Left $ LinkAcceptTOS
        Nothing -> case (ctxcompany ctx) of
             Just _company -> Right <$> action
             Nothing -> return $ Left $ LinkLogin (ctxlocale ctx) NotLogged

randomPassword :: (MonadIO m, CryptoRNG m) => m BS.ByteString
randomPassword =
    BS.fromString `liftM` randomString 8 (['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z'])
