-- | Basic support for QR-encoding strings into .png images. Requires
-- 'qrencode' and 'zbarimg' from 'zbar-tools' in $PATH ('zbarimg' is
-- only required for decoding -- in other words, by the test suite).

module Util.QRCode ( QRCode(..)
                   , encodeQR
                   , decodeQR
                   , decodeQRBSL ) where

import Data.String.Utils (rstrip)
import System.IO
import System.IO.Temp
import System.Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Happstack.Server.Response as Web
import qualified Happstack.Server.Types as Web

import API.V2.Monad

newtype QRCode = QRCode { unQRCode :: BS.ByteString }

instance ToAPIResponse QRCode where
  toAPIResponse (QRCode bsdata) =
    Web.setHeader "Content-Type" "image/png" $ Web.toResponse bsdata

encodeQR :: String -> IO QRCode
encodeQR msg = withSystemTempFile "qr.png" $ \path handle -> do
  hClose handle
  callProcess
    "qrencode"
    [ msg
    , "-o"
    , path
    , "--size"
    , "20"
                         -- Makes a 580x580px ~0.5 KiB image.
    ]
  QRCode <$> BS.readFile path

decodeQR :: QRCode -> IO String
decodeQR (QRCode bsdata) = withSystemTempFile "qr.png" $ \path handle -> do
  BS.hPut handle bsdata
  hClose handle
  zbarout <- readProcess "zbarimg" ["-q", path] ""
  case break (== ':') zbarout of
    ("QR-Code", ':' : code) -> return $ rstrip code
    _ -> unexpectedError "QR code couldn't be decoded."

decodeQRBSL :: BSL.ByteString -> IO String
decodeQRBSL = decodeQR . QRCode . BSL.toStrict
