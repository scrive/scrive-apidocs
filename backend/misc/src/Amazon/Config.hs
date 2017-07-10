module Amazon.Config (AmazonConfig) where

import KontraPrelude (String)

-- | AWS config: (bucket, access key, secret key).
type AmazonConfig = (String,String,String)
