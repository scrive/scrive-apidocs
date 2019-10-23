module PasswordService.Control
  ( checkPassword
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Data.Char
import Log
import System.Exit
import qualified Crypto.Hash as H
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.UTF8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Log.Utils
import PasswordService.Conf
import Utils.IO

checkPassword
  :: (MonadBase IO m, MonadLog m, MonadThrow m) => PasswordServiceConf -> Text -> m Bool
checkPassword config pwd = if (T.length pwd < 12)
  then do
    logInfo_ "Password is too short"
    return False
  else do
    let sha1           = map toUpper . show . H.hashWith H.SHA1 $ TE.encodeUtf8 pwd
        (sha1p, sha1r) = splitAt 5 sha1
        url            = (passwordServiceUrl config) ++ "/range/" ++ sha1p
    (exitcode, stdout, _stderr) <- readCurl
      [ "--tlsv1.2"
      , "-L" -- make curl follow redirects
      , "--post302" -- make curl still post after redirect
      , url
      ]
      BSL.empty
    case (exitcode) of
      ExitSuccess -> do
        if (any (BS.isPrefixOf $ BSC.fromString sha1r) (BSC.lines (BSL.toStrict stdout)))
          then do
            logInfo "Password service reported hash as compromised"
              $ object ["sha" .= sha1p]
            return False
          else do
            logInfo "Password service confirmed that password is good"
              $ object ["sha" .= sha1p]
            return True
      ExitFailure err -> do
        logAttention
            "Password service didn't work as expected. This should be investigated, \
          \however we will accept all passowrds now"
          $ object ["err" .= show err, "stdout" `equalsExternalBSL` stdout]
        return True
