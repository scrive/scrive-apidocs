module Network.SOAP.Call (
    xpSOAPFault
  , soapCall
  , unwrapEnvelope
  ) where

import Control.Monad.Base
import Control.Monad.Catch
import Log
import Network.SOAP
import Network.SOAP.Parsing.Cursor
import Text.XML
import Text.XML.Cursor hiding (element)

import Text.XML.Parser
import Text.XML.Writer.Extended hiding (empty, node)

-- | SOAPFault parser.
xpSOAPFault :: XMLParser SOAPFault
xpSOAPFault = XMLParser $ \c -> listToMaybe $ c $/ laxElement "Fault" &| \fault ->
  SOAPFault { faultCode   = readT "faultcode" fault
            , faultString = readT "faultstring" fault
            , faultDetail = readT "detail" fault
            }

----------------------------------------

-- | Make a SOAP call.
soapCall
  :: (ToXML header, ToXML body, MonadBase IO m, MonadLog m, MonadThrow m)
  => Transport
  -> String
  -> header
  -> body
  -> XMLParser response
  -> m response
soapCall transport soap_action header body parser = do
  logInfo "Making SOAP call" $ object ["request_body" .= renderText def req]
  c <-
    liftBase (transport soap_action req) >>= unwrapEnvelope . fromDocument . parseLBS_ def
  case runParser (Right <$> parser <|> Left <$> xpSOAPFault) c of
    Just (Right response) -> return response
    Just (Left  fault   ) -> throwM fault
    Nothing               -> throwM
      $ SOAPParsingError ("Unsuccessful parse of SOAP response body:" <+> ppCursor c)
  where req = soap header body

----------------------------------------

-- | Extract SOAP body from the envelope.
unwrapEnvelope :: MonadThrow m => Cursor -> m Cursor
unwrapEnvelope c =
  maybe err return . listToMaybe $ c $| laxElement "Envelope" &/ laxElement "Body"
  where err = throwM . SOAPParsingError $ "No SOAP Body:" <+> ppCursor c
