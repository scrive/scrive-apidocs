-- | Support for parsing, rendering and simple processing of XML content.
module Text.XML.Content
  ( XMLContent(..)
  , parseXMLContent
  , renderXMLContent
  , substitute
  , removeTags
  , cdata
  , ToXMLContent(..)
  , XMLSubstitutionT(..)
  , runXMLSubstitutionT
  , (.=)
  )
  where

import Control.Exception (SomeException)
import Control.Monad.ST (runST)
import Control.Monad.State.Strict
import Data.Conduit ((.|), runConduit)
import Data.Conduit.List (consume, sourceList)
import Data.Default (def)
import Data.Map (Map)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Lazy (fromStrict)
import Data.XML.Types (Content(..), Event(..))
import Text.XML
  ( Element(..), Name, Node(..), decodeHtmlEntities, documentRoot, elementNodes
  , parseText, psDecodeEntities
  )
import Text.XML.Stream.Render (renderBytes)
import qualified Data.Map as Map
import qualified Data.Text as T

newtype XMLContent = XMLContent { unXMLContent :: [Node] }
  deriving (Eq, Ord, Show)

-- | Parses text as XML; removing processing instructions and comments; cleaning up HTML entities
parseXMLContent :: Text -> Either SomeException XMLContent
parseXMLContent s =
  XMLContent . concatMap cleanup . elementNodes . documentRoot <$> parseText
    def { psDecodeEntities = decodeHtmlEntities }
    (fromStrict ("<a>" `T.append` s `T.append` "</a>"))
  where
    cleanup NodeInstruction{} = []
    cleanup NodeComment{}     = []
    cleanup (NodeElement (Element n as ns)) =
      [NodeElement (Element n as (concatMap cleanup ns))]
    cleanup n = [n]

renderXMLContent :: XMLContent -> Text
renderXMLContent ns = T.concat . map decodeUtf8 $ runST
  (runConduit $ sourceList (nodesToEvents ns) .| renderBytes def .| consume)

nodesToEvents :: XMLContent -> [Event]
nodesToEvents ct = goN (unXMLContent ct) []
  where
    goE (Element name as ns) =
      (EventBeginElement name (Map.toList (fmap ((: []) . ContentText) as)) :)
        . goN ns
        . (EventEndElement name :)
    goN []       = identity
    goN [x     ] = goN' x
    goN (x : xs) = goN' x . goN xs
    goN' (NodeElement     e) = goE e
    goN' (NodeInstruction i) = (EventInstruction i :)
    goN' (NodeContent     c) = (EventContent (ContentText c) :)
    goN' (NodeComment     t) = (EventComment t :)

substitute :: Map (Name, Text) XMLContent -> XMLContent -> XMLContent
substitute m = XMLContent . concatMap subN . unXMLContent
  where
    subN (NodeElement (Element e as _))
      | Just i <- Map.lookup "class" as, Just (XMLContent ns) <- Map.lookup (e, i) m = ns
    subN (NodeElement (Element e as ns)) =
      [NodeElement (Element e as (concatMap subN ns))]
    subN n = [n]

-- | Remove tags, keeping content
removeTags :: XMLContent -> XMLContent
removeTags = XMLContent . concatMap remove . unXMLContent
  where
    remove (NodeElement (Element _ _ ns)) = concatMap remove ns
    remove n@NodeContent{}                = [n]
    remove _                              = []

-- | Form XMLContent from string literals
instance IsString XMLContent where
  fromString s =
    either (unexpectedError $ "Cannot parse XML content " <> showt s) identity
      $ parseXMLContent (T.pack s)

-- | Form XMLContent from plain text
cdata :: Text -> XMLContent
cdata t = XMLContent [NodeContent t]

-- | Stuff that can be converted into XML content
class ToXMLContent a where
  toXMLContent :: a -> XMLContent

instance ToXMLContent XMLContent where
  toXMLContent = identity

instance ToXMLContent Text where
  toXMLContent = cdata

instance ToXMLContent String where
  toXMLContent = toXMLContent . T.pack

-- | Substitution monad
newtype XMLSubstitutionT m a =
  XMLSubstitutionT { unXMLSubstitutionT :: StateT (Map (Name, Text) XMLContent) m a }
  deriving (Applicative, Functor, Monad, MonadTrans)


(.=) :: (Monad m, ToXMLContent a) => Text -> a -> XMLSubstitutionT m ()
n .= v = XMLSubstitutionT $ modify (Map.insert ("span", n) (toXMLContent v))

runXMLSubstitutionT :: Monad m => XMLSubstitutionT m () -> m (Map (Name, Text) XMLContent)
runXMLSubstitutionT (XMLSubstitutionT m) = execStateT m Map.empty
