module Network.SOAP.Call (
    XMLParser(..)
  , xpSOAPFault
  , soapCall
  , unwrapEnvelope
  , ppDocument
  , ppCursor
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Data.Maybe
import Data.Monoid.Utils
import Network.SOAP
import Network.SOAP.Parsing.Cursor
import Text.XML
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (empty, node)
import qualified Data.Text.Lazy as T
import qualified Text.XML.Writer as W

import qualified Log

-- | Lightweight wrapper over cursor parsers to allow for
-- convenient transformation and combining the results.
-- TODO: Improve logging capabilities (some thought needs to
-- be given as a parser can either produce no results or fail,
-- currently 'Maybe' stands for "produces no results" and failures
-- are generally unexpected and handled with 'unexpectedError').
newtype XMLParser a = XMLParser { runParser :: Cursor -> Maybe a }
  deriving Functor

instance Applicative XMLParser where
  pure = return
  (<*>) = ap

instance Alternative XMLParser where
  empty = mzero
  (<|>) = mplus

instance Monad XMLParser where
  return = XMLParser . const . Just
  XMLParser f >>= g = XMLParser $ \c -> f c >>= ($ c) . runParser . g

instance MonadPlus XMLParser where
  mzero = XMLParser $ const Nothing
  XMLParser f `mplus` XMLParser g = XMLParser $ \c -> f c `mplus` g c

-- | SOAPFault parser.
xpSOAPFault :: XMLParser SOAPFault
xpSOAPFault = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "Fault" &| \fault -> SOAPFault {
    faultCode   = readT "faultcode" fault
  , faultString = readT "faultstring" fault
  , faultDetail = readT "detail" fault
  }

----------------------------------------

-- | Make a SOAP call.
soapCall :: (ToXML header, ToXML body, MonadBase IO m, Log.MonadLog m, MonadThrow m)
         => Transport
         -> String
         -> header
         -> body
         -> XMLParser response
         -> m response
soapCall transport soap_action header body parser = do
  Log.mixlog_ $ "SOAP request body:" <+> ppDocument req
  c <- liftBase (transport soap_action req)
    >>= unwrapEnvelope . fromDocument . parseLBS_ def
  case runParser (Right <$> parser <|> Left <$> xpSOAPFault) c of
    Just (Right response) -> return response
    Just (Left fault) -> throwM fault
    Nothing -> throwM $ SOAPParsingError $ "Unsuccessful parse of SOAP response body:" <+> ppCursor c
  where
    req = soap header body

----------------------------------------

-- | Extract SOAP body from the envelope.
unwrapEnvelope :: MonadThrow m => Cursor -> m Cursor
unwrapEnvelope c = maybe err return . listToMaybe
  $ c $| laxElement "Envelope" &/ laxElement "Body"
  where
    err = throwM . SOAPParsingError $ "No SOAP Body:" <+> ppCursor c

-- | Pretty print XML document.
ppDocument :: Document -> String
ppDocument = T.unpack . renderText def

-- | Render cursor as an XML document and pretty print it.
ppCursor :: Cursor -> String
ppCursor = ppDocument . document "response" . W.node . node
