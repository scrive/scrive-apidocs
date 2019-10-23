{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instance mappings between hpqtypes XML and 'Text.XML.Content' and 'Text.XML.DirtyContent'
module DB.XML () where

import Control.Exception (throwIO)
import Data.Text (Text)
import Database.PostgreSQL.PQTypes

import qualified Text.XML.Content as C
import qualified Text.XML.DirtyContent as D

instance PQFormat C.XMLContent where
  pqFormat = pqFormat @XML

instance FromSQL C.XMLContent where
  type PQBase C.XMLContent = PQBase XML
  fromSQL mbase = either throwIO return =<< (C.parseXMLContent . unXML <$> fromSQL mbase)

instance ToSQL C.XMLContent where
  type PQDest C.XMLContent = PQDest XML
  toSQL = toSQL . XML . C.renderXMLContent

instance PQFormat D.XMLContent where
  pqFormat = pqFormat @Text

instance FromSQL D.XMLContent where
  type PQBase D.XMLContent = PQBase Text
  fromSQL mbase = D.parseXMLContent <$> fromSQL mbase

instance ToSQL D.XMLContent where
  type PQDest D.XMLContent = PQDest Text
  toSQL = toSQL . D.renderXMLContent
