module Auth.OAuth where

import Control.Conditional ((<|), (|>))
import qualified Data.Text as T

import Auth.Model
import Auth.Parse
import Auth.Utils

-- Read authorization header for oauth.
parseParams :: [(Text, Text)] -> Either Text OAuthAuthorization
parseParams params =
  let msigtype   = lookupAndRead "oauth_signature_method" params :: Maybe Text
      mapisecret = splitSignature =<< lookupAndRead "oauth_signature" params
      mapitoken  = lookupAndReadString "oauth_consumer_key" params
      macctoken  = lookupAndReadString "oauth_token" params
      errors     = T.intercalate
        "; "
        (  ["oauth_signature_method must be 'PLAINTEXT'"]
        <| Just "PLAINTEXT"
        /= msigtype
        |> []
        <> ["oauth_signature was missing or in bad format"]
        <| isNothing mapisecret
        |> []
        <> ["oauth_signature api secret (first param) is missing"]
        <| isNothing (fst =<< mapisecret)
        |> []
        <> ["oauth_signature token secret (second param) is missing"]
        <| isNothing (snd =<< mapisecret)
        |> []
        <> ["oauth_consumer_key is missing or is invalid"]
        <| isNothing mapitoken
        |> []
        <> ["oauth_token is required"]
        <| isNothing macctoken
        |> []
        )
  in  if not $ T.null errors
        then Left errors
        else Right $ OAuthAuthorization
          { oaAPIToken     = fromJust mapitoken
          , oaAPISecret    = fromJust . fst $ fromJust mapisecret
          , oaAccessToken  = fromJust macctoken
          , oaAccessSecret = fromJust . snd $ fromJust mapisecret
          }
