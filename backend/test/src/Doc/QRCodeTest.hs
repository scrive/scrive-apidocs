module Doc.QRCodeTest where

import Control.Monad
import Control.Monad.IO.Class
import Crypto.RNG.Utils
import Test.Framework

import Doc.QRCode
import KontraPrelude
import TestingUtil
import TestKontra

qrCodeTests :: TestEnvSt -> Test
qrCodeTests env = testGroup "QRCode" [
  testThat "QR code roundtrip" env qrCodeRoundtrip
  ]

qrCodeRoundtrip :: TestEnv ()
qrCodeRoundtrip = replicateM_ 10 $ do
  str <- randomString 64 $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
  enc <- liftIO $ encodeQR str
  dec <- liftIO $ decodeQR enc
  assertEqual "QR code roundtripping should work" str dec
