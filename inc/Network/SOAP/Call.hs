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
import Data.Monoid.Space
import Network.SOAP
import Network.SOAP.Parsing.Cursor
import Text.XML
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (empty, node)
import qualified Data.Text.Lazy as T
import qualified Text.XML.Writer as W

newtype XMLParser a = XMLParser { runParser :: Cursor -> Maybe a }
  deriving Functor

instance Applicative XMLParser where
  pure = XMLParser . const . Just
  XMLParser f <*> XMLParser g = XMLParser $ \c -> f c >>= (<$> g c)

instance Alternative XMLParser where
  empty = XMLParser $ const Nothing
  XMLParser f <|> XMLParser g = XMLParser $ \c -> f c <|> g c

instance Monad XMLParser where
  return = pure
  XMLParser f >>= g = XMLParser $ \c -> f c >>= ($ c) . runParser . g

instance MonadPlus XMLParser where
  mzero = empty
  mplus = (<|>)

xpSOAPFault :: XMLParser SOAPFault
xpSOAPFault = XMLParser $ \c -> listToMaybe $ c
  $/ laxElement "Fault" &| \fault -> SOAPFault {
    faultCode   = readT "faultcode" fault
  , faultString = readT "faultstring" fault
  , faultDetail = readT "detail" fault
  }

----------------------------------------

soapCall :: (ToXML header, ToXML body, MonadBase IO m)
         => Transport
         -> String
         -> header
         -> body
         -> XMLParser response
         -> m response
soapCall transport soap_action header body parser = liftBase $ do
  c <- transport soap_action (soap header body)
    >>= unwrapEnvelope . fromDocument . parseLBS_ def
  case runParser (Right <$> parser <|> Left <$> xpSOAPFault) c of
    Just (Right response) -> return response
    Just (Left fault) -> throwM fault
    Nothing -> throwM $ SOAPParsingError $ "SOAP Body parsing failed:" <+> ppCursor c

----------------------------------------

unwrapEnvelope :: MonadThrow m => Cursor -> m Cursor
unwrapEnvelope c = maybe err return . listToMaybe
  $ c $| laxElement "Envelope" &/ laxElement "Body"
  where
    err = throwM . SOAPParsingError $ "No SOAP Body:" <+> ppCursor c

ppDocument :: Document -> String
ppDocument = T.unpack . renderText def

ppCursor :: Cursor -> String
ppCursor = ppDocument . document "response" . W.node . node
