module Doc.JpegPages

where

import qualified Data.ByteString as BS
import Happstack.Data

data JpegPages0 = JpegPagesPending0
               | JpegPages0 [BS.ByteString]
               | JpegPagesError0 BS.ByteString
    deriving (Eq, Ord, Typeable)

data JpegPages = JpegPagesPending
               | JpegPages [(BS.ByteString,Int,Int)]  -- Data + width + height (scaled with some resolution)
               | JpegPagesError BS.ByteString
    deriving (Eq, Ord, Typeable)

instance Show JpegPages where
    show JpegPagesPending = "penging"
    show (JpegPages l) = show l
    show (JpegPagesError c) = "error " ++ (show c)

$(deriveSerialize ''JpegPages0)
instance Version JpegPages0 where

$(deriveSerialize ''JpegPages)
instance Version JpegPages where
    mode = extension 1 (Proxy :: Proxy JpegPages0)

instance Migrate JpegPages0 JpegPages where
    migrate JpegPagesPending0 = JpegPagesPending
    migrate (JpegPagesError0 x) = JpegPagesError x
    migrate (JpegPages0 l) = JpegPages $ map (\x -> (x,943,1335)) l

