module Util.Zlib where

import Data.Either ()
import Codec.Compression.Zlib.Internal
import qualified Data.ByteString.Lazy.Char8 as LB

-- | Convert & unfold the custom DecompressStream
--   error format from zlib to a Either
decompressStreamToEither :: DecompressStream -> Either String LB.ByteString
decompressStreamToEither (StreamError _ errmsg) = Left errmsg
decompressStreamToEither stream@(StreamChunk _ _) = Right $ fromDecompressStream stream
decompressStreamToEither StreamEnd = Right $ ""

-- | Decompress with explicit error handling
safeDecompress :: LB.ByteString -> Either String LB.ByteString
safeDecompress bstr = decompressStreamToEither $ decompressWithErrors gzipOrZlibFormat defaultDecompressParams bstr

-- | Decompress gzip, if it fails, return uncompressed String
decompressIfPossible :: LB.ByteString -> LB.ByteString
decompressIfPossible bstr =
    let conv (Left _) = bstr
        conv (Right a) = a
    in (conv . safeDecompress) bstr
