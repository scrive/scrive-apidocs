{-

All function related to Amazon Web Services

-}

module Amazon 
    ( module Network.AWS.Authentication
    , urlFromFile
    , uploadFile
    )
where

import DocState
import Network.AWS.Authentication
import qualified Network.AWS.Authentication as AWS
import qualified Network.HTTP as HTTP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL hiding (length)
import qualified Data.ByteString.UTF8 as BS hiding (length)

urlFromFile :: File -> String
urlFromFile File{filename,fileid} =
    show fileid ++ "/" ++ HTTP.urlEncode (BS.toString filename) ++ ".pdf"


uploadFile ctxs3action file@File{filestoragemode = FileStorageLocal} = do
  let action = ctxs3action { AWS.s3object = urlFromFile file
                           , AWS.s3operation = HTTP.PUT
                           , AWS.s3body = BSL.fromChunks [filepdf file]
                           }
  result <- AWS.runAction action
  case result of
    Right _ -> return ()
    -- FIXME: do much better error handling
    Left err -> print err >> return ()
uploadFile _ _ = return ()
  