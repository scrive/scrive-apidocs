module Text.XML.Parser (
    XMLParser(..)
  , ppCursor
  ) where

import Text.XML
import Text.XML.Cursor hiding (element)
import Text.XML.Writer hiding (empty, node)
import qualified Data.Text.Lazy as T
import qualified Text.XML.Writer as W

-- | Lightweight wrapper over cursor parsers to allow for
-- convenient transformation and combining the results.
-- TODO: Improve logging capabilities (some thought needs to
-- be given as a parser can either produce no results or fail,
-- currently 'Maybe' stands for "produces no results" and failures
-- are generally unexpected and handled with 'unexpectedError').
newtype XMLParser a = XMLParser { runParser :: Cursor -> Maybe a }
  deriving Functor

instance Applicative XMLParser where
  pure  = return
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

-- | Render cursor as an XML document and pretty print it.
ppCursor :: Cursor -> String
ppCursor = T.unpack . renderText def . document "response" . W.node . node
